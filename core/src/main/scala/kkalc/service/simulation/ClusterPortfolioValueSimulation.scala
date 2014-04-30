package kkalc.service.simulation

import akka.actor._
import akka.cluster.ClusterEvent.{CurrentClusterState, MemberUp}
import akka.cluster.{Member, MemberStatus, Cluster}
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.typesafe.config.Config

import java.util.concurrent.Executors

import kkalc.model.{Position, Portfolio}
import kkalc.pricing.{MarketFactorsGenerator, PortfolioPricer}
import kkalc.service.MarketFactorsModule

import messages._

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Promise, Future}
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import scalaz.NonEmptyList.nel
import scalaz.concurrent.Task
import scalaz.stream.io._
import scalaz.{-\/, \/-}


object messages {
  case object WakeUp

  case object RegisterBackend

  case class RunSimulation(positions: List[Position], simulations: Int, generator: MarketFactorsGenerator)

  case class SimulationResponse(v: Vector[Double])
}

trait ClusterPortfolioValueSimulation extends PortfolioValueSimulation {
  self: MarketFactorsModule with MonteCarloMarketRiskCalculator =>

  def systemName: String

  def systemConfig: Config

  private[this] lazy val system = ActorSystem(systemName, systemConfig)
  private[this] lazy val backendManager = system.actorOf(Props[BackendNodesManager], "backendManager")

  private[this] implicit val timeout = Timeout(10.seconds)

  def join(address: Address) {
    Cluster(system).join(address)
    backendManager ! WakeUp
  }

  def shutdown() {
    system.shutdown()
  }

  def simulation(portfolio: Portfolio, simulations: Int) = channel[MarketFactorsGenerator, Simulations] {
    generator =>
      import scala.concurrent.ExecutionContext.Implicits.global

      Task.async[Simulations](cb => {
        (backendManager ? RunSimulation(portfolio.positions.list, simulations, generator)).onComplete {
          case Failure(err)                   => cb(-\/(err))
          case Success(SimulationResponse(s)) => cb(\/-(Simulations(s)))
          case Success(u)                     => cb(-\/(new IllegalStateException(s"Unsupported backend response: $u")))
        }
      })
  }
}

/**
 * Manage available simulation backend nodes
 */
private[simulation] class BackendNodesManager extends Actor with ActorLogging {

  import scala.concurrent.ExecutionContext.Implicits.global

  private[this] val backendNodes = ListBuffer.empty[ActorRef]
  private[this] var jobCounter = 0

  private[this] implicit val timeout = Timeout(10.seconds)

  override def receive: Receive = {
    case WakeUp => log.info("Wake up backend nodes manager")

    case run: RunSimulation =>
      jobCounter += 1
      val backendN = jobCounter % backendNodes.size
      log.debug(s"Pass simulation request to backend: $backendN")
      backendNodes(backendN) ? run pipeTo sender()

    case RegisterBackend if !backendNodes.contains(sender()) =>
      context watch sender()
      backendNodes += sender()
      log.debug(s"Added new backend. Total: ${backendNodes.size}. Node: [${sender()}}]")

    case Terminated(backEnd) if backendNodes.contains(backEnd) =>
      backendNodes -= sender()
      log.debug(s"Removed terminated backend. Total: ${backendNodes.size}. Terminated node: [${sender()}}]")
  }
}

/**
 * Backend actor that runs Portfolio Simulation
 */
class PortfolioValueSimulationBackend extends Actor with ActorLogging {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val cluster = Cluster(context.system)

  private lazy val executor = {
    val threadFactory = new ThreadFactoryBuilder().setNameFormat("simulation-pool-%d").setDaemon(true).build()
    Executors.newCachedThreadPool(threadFactory)
  }

  override def preStart(): Unit = cluster.subscribe(self, classOf[MemberUp])

  override def postStop(): Unit = cluster.unsubscribe(self)

  def receive = {
    case RunSimulation(portfolio, simulations, generator) =>
      runSimulation(Portfolio(nel(portfolio.head, portfolio.tail)), simulations, generator) pipeTo sender()

    case state: CurrentClusterState =>
      state.members.filter(_.status == MemberStatus.Up) foreach register

    case MemberUp(member) =>
      register(member)
  }

  private def register(member: Member): Unit =
    if (member.hasRole("calculator")) {
      val backendManager = context.actorSelection(RootActorPath(member.address) / "user" / "backendManager")
      backendManager ! RegisterBackend
    }

  private def runSimulation(portfolio: Portfolio, simulations: Int, generator: MarketFactorsGenerator): Future[SimulationResponse] = {
    val process = generator.factors.take(simulations).map {
      implicit factors =>
        PortfolioPricer.price(portfolio).fold(err => sys.error(s"Failed to price portfolio: $err"), identity)
    }

    // Fork simulations into separate thread pool
    val task = Task.fork {
      log.debug(s"Simulate $simulations portfolio values for [${portfolio.positions.list.mkString(", ")}]")
      process.runLog.map(portfolioValues => portfolioValues.toVector)
    }(executor)

    val p = Promise[SimulationResponse]()

    task.runAsync {
      case -\/(err) => p.failure(err)
      case \/-(result) => p.success(SimulationResponse(result))
    }

    p.future
  }
}