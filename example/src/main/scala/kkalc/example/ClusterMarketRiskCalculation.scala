package kkalc.example

import akka.actor.{Props, ActorSystem}
import akka.cluster.Cluster
import com.typesafe.config.ConfigFactory
import kkalc.service.historical.{HistoricalMarketData, HistoricalMarketFactors}
import kkalc.service.simulation.{ClusterPortfolioValueSimulation, MonteCarloMarketRiskCalculator, PortfolioValueSimulationBackend}
import org.slf4j.LoggerFactory

object ClusterMarketRiskCalculation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

  val SystemName = "SimulationNode"

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

  Thread.sleep(2000)

  // Start Cluster Risk Calculator node
  object RiskCalculator
    extends MonteCarloMarketRiskCalculator(10000, 10)
    with ClusterPortfolioValueSimulation
    with HistoricalMarketFactors with HistoricalMarketData {

    val systemName = SystemName
    val systemConfig = calculatorConfig
  }

  RiskCalculator.join(joinAddress)

}