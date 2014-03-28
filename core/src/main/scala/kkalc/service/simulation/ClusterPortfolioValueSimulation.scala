package kkalc.service.simulation

import akka.actor.{Address, Actor, ActorSystem}
import akka.cluster.Cluster
import com.typesafe.config.Config
import kkalc.model.Portfolio
import kkalc.pricing.PortfolioPricer
import kkalc.service.MarketFactorsModule
import org.slf4j.LoggerFactory
import scalaz.concurrent.Task
import scalaz.stream.io._

trait ClusterPortfolioValueSimulation extends PortfolioValueSimulation {
  self: MarketFactorsModule with MonteCarloMarketRiskCalculator =>

  def systemName: String

  def systemConfig: Config

  private[this] val log = LoggerFactory.getLogger(classOf[ClusterPortfolioValueSimulation])

  private[this] lazy val system = ActorSystem(systemName, systemConfig)

   // Join cluster Manually
  def join(address: Address) {
    Cluster(system).join(address)
  }

  // Shutdown actor system
  def shutdown() {
    system.shutdown()
  }

  def simulation(portfolio: Portfolio, simulations: Int) = channel[MarketFactorsGenerator, Simulations] {
    generator =>
      val process = generator.factors.take(simulations).map {
        implicit factors =>
          PortfolioPricer.price(portfolio).fold(err => sys.error(s"Failed to price portfolio: $err"), identity)
      }

      // Fork simulations into separate thread pool
      Task.fork {
        log.debug(s"Simulate $simulations portfolio values for $portfolio")
        process.runLog.map(portfolioValues => Simulations(portfolioValues.toVector))
      }
  }
}

/**
 * Manage available simulation backend nodes
 */
private[simulation] class SimulationNodesManager extends Actor {
  override def receive: Receive = ???
}