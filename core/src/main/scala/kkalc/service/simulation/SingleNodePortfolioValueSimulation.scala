package kkalc.service.simulation

import com.google.common.util.concurrent.ThreadFactoryBuilder

import java.util.concurrent.Executors

import kkalc.model.Portfolio
import kkalc.pricing.{MarketFactorsGenerator, PortfolioPricer}
import kkalc.service.MarketFactorsModule

import org.slf4j.LoggerFactory

import scalaz.concurrent.Task
import scalaz.stream.io.channel

trait SingleNodePortfolioValueSimulation extends PortfolioValueSimulation {
  self: MarketFactorsModule with MonteCarloMarketRiskCalculator =>

  private[this] val log = LoggerFactory.getLogger(classOf[SingleNodePortfolioValueSimulation])

  private[this] lazy val executor = {
    val threadFactory = new ThreadFactoryBuilder().setNameFormat("simulation-pool-%d").setDaemon(true).build()
    Executors.newCachedThreadPool(threadFactory)
  }

  /**
   * Scalaz-Stream Channel that for given market factors generator
   * runs defined number of simulations and produce MarketRisk
   */
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
      }(executor)
  }
}