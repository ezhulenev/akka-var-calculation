package kkalc.service

import kkalc.model.Portfolio
import kkalc.pricing.MarketFactors
import org.joda.time.LocalDate
import scalaz.concurrent.Task
import scalaz.stream.Process

trait MarketFactorsModule {

  case class MarketFactorsParameters(riskFreeRate: Double = 0.05, horizon: Int = 1000)

  trait MarketFactorsGenerator {
    def factors: Process[Task, MarketFactors]
  }

  /**
   * Generate one-day forecast for Market Factors using historical price correlation
   */
  protected def oneDayMarketFactors(portfolio: Portfolio, date: LocalDate)(implicit parameters: MarketFactorsParameters): MarketFactorsGenerator

  /**
   * Get Market Factors at given date
   */
  protected def marketFactors(date: LocalDate)(implicit parameters: MarketFactorsParameters): MarketFactors
}