package kkalc.service.simulation

import kkalc.model.Portfolio
import kkalc.pricing.PortfolioPricer
import kkalc.service.MarketFactorsModule
import org.slf4j.LoggerFactory
import scalaz.stream.io.channel

trait LocalPortfolioValueSimulation extends PortfolioValueSimulation {
  self: MarketFactorsModule with MonteCarloMarketRiskCalculator =>

  private[this] val log = LoggerFactory.getLogger(classOf[LocalPortfolioValueSimulation])

  /**
   * Scalaz-Stream Channel that for given market factors generator
   * runs defined number of simulations and produce MarketRisk
   */
  def simulation(portfolio: Portfolio, simulations: Int) = channel[MarketFactorsGenerator, Simulations] {
    generator =>
      log.debug(s"Simulate $simulations portfolio values for $portfolio")

      val process = generator.factors.take(simulations).map {
        implicit factors =>
          PortfolioPricer.price(portfolio).fold(err => sys.error(s"Failed to price portfolio: $err"), identity)
      }
      process.runLog.map(portfolioValues => Simulations(portfolioValues.toVector))
  }
}