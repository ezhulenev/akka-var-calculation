package kkalc.example

import kkalc.model.{CallOption, Position, Portfolio, Equity}
import scalaz.NonEmptyList.nels
import kkalc.service.simulation.{LocalPortfolioValueSimulation, MonteCarloMarketRiskCalculator}
import kkalc.service.historical.{HistoricalMarketData, HistoricalMarketFactors}
import org.joda.time.LocalDate
import org.slf4j.LoggerFactory

/**
 * Run MarketRisk calculation locally in single JVM
 */
object LocalMarketRiskCalculation extends App {
  private val log = LoggerFactory.getLogger(this.getClass)

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



  object RiskCalculator
    extends MonteCarloMarketRiskCalculator(10000, 10)
    with LocalPortfolioValueSimulation
    with HistoricalMarketFactors with HistoricalMarketData


  val start = System.currentTimeMillis()
  val marketRisk = RiskCalculator.marketRisk(portfolio, date)
  val end = System.currentTimeMillis()

  log.info(s"Calculated marker risk in ${end - start} milliseconds; " +
    s"VaR(p = 0.95) = ${marketRisk.VaR(0.95)}, " +
    s"CVaR(p = 0.95) = ${marketRisk.conditionalVaR(0.95)}")

  System.exit(0)

}