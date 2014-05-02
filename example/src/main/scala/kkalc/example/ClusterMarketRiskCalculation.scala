package kkalc.example

import akka.actor.{Props, ActorSystem}
import akka.cluster.Cluster

import com.typesafe.config.ConfigFactory

import kkalc.model._
import kkalc.service.historical.{HistoricalMarketData, HistoricalMarketFactors}
import kkalc.service.simulation.{ClusterPortfolioValueSimulation, MonteCarloMarketRiskCalculator, PortfolioValueSimulationBackend}

import org.joda.time.LocalDate

import scalaz.NonEmptyList._

object ClusterMarketRiskCalculation extends App {
  val SystemName = "ClusterMarketRiskCalculation"

  val simulationConfig = ConfigFactory.parseResources("simulation-node.conf")
  val calculatorConfig = ConfigFactory.parseResources("calculator-node.conf")

  // Start 3 simulation nodes
  val system1  = ActorSystem(SystemName, simulationConfig)
  val joinAddress = Cluster(system1).selfAddress
  Cluster(system1).join(joinAddress)
  system1.actorOf(Props[PortfolioValueSimulationBackend], "simulationBackend")

  val system2  = ActorSystem(SystemName, simulationConfig)
  Cluster(system2).join(joinAddress)
  system2.actorOf(Props[PortfolioValueSimulationBackend], "simulationBackend")

  val system3  = ActorSystem(SystemName, simulationConfig)
  Cluster(system3).join(joinAddress)
  system3.actorOf(Props[PortfolioValueSimulationBackend], "simulationBackend")

  // Start Cluster Risk Calculator node
  object RiskCalculator
    extends MonteCarloMarketRiskCalculator(10000, 10)
    with ClusterPortfolioValueSimulation
    with HistoricalMarketFactors with HistoricalMarketData {

    val systemName = SystemName
    val systemConfig = calculatorConfig
  }

  RiskCalculator.join(joinAddress)

  // Let's cluster state some time to converge
  Thread.sleep(2000)

  // Run VaR calculation

  val AMZN = Equity("AMZN")
  val AAPL = Equity("AAPL")
  val IBM = Equity("IBM")
  val GS = Equity("GS")

  // Portfolio evaluation date
  val date = new LocalDate(2014, 1, 3)

  // Options maturity date
  val maturityDate = new LocalDate(2014, 3, 31)

  val portfolio = Portfolio(nels(
    Position(AMZN, 10),
    Position(AAPL, 20),
    Position(IBM, 30),
    Position(CallOption(GS, 180, maturityDate), 10)
  ))

  val start = System.currentTimeMillis()
  val marketRisk = RiskCalculator.marketRisk(portfolio, date)
  val end = System.currentTimeMillis()

  println(s"Calculated marker risk in ${end - start} milliseconds; " +
    s"VaR(p = 0.95) = ${marketRisk.VaR(0.95)}, " +
    s"CVaR(p = 0.95) = ${marketRisk.conditionalVaR(0.95)}")

  // Shutdown actor systems
  system1.shutdown()
  system2.shutdown()
  system3.shutdown()
  RiskCalculator.shutdown()

  // and application
  System.exit(0)
}