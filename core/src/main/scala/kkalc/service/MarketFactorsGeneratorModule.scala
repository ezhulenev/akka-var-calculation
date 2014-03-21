package kkalc.service

import kkalc.pricing.MarketFactors
import org.joda.time.LocalDate
import scalaz.concurrent.Task
import scalaz.stream.Process
import kkalc.model.Portfolio

trait MarketFactorsGeneratorModule {

  case class GeneratorConfig(riskFreeRate: Double = 0.05, horizon: Int = 1000)

  def oneDayMarketFactors(date: LocalDate, portfolio: Portfolio)(implicit config: GeneratorConfig): MarketFactorsGenerator

  /**
   * Generate market factors required for pricing given portfolio
   */
  trait MarketFactorsGenerator {
    def factors: Process[Task, MarketFactors]
  }
}