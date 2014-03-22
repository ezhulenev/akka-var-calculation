package kkalc.service

import kkalc.model.{Position, Portfolio, Equity}
import kkalc.pricing.MarketFactor.Price
import kkalc.pricing.MarketFactors
import kkalc.service.historical.HistoricalMarketFactorsGenerator
import org.joda.time.LocalDate
import org.scalatest.FlatSpec
import org.slf4j.LoggerFactory
import scalaz.NonEmptyList.nels

class OneDayMarketFactorsGeneratorSpec extends FlatSpec with HistoricalMarketFactorsGenerator {
  private val log = LoggerFactory.getLogger(classOf[OneDayMarketFactorsGeneratorSpec])

  val AAPL = Equity("AAPL")
  val AMZN = Equity("AMZN")
  val GS = Equity("GS")

  val portfolio = Portfolio(nels(Position(AAPL, 10), Position(AMZN, 20), Position(GS, 30)))

  implicit val config = GeneratorConfig()

  "Market Factors Generator" should "correctly generate one day market factors for portfolio" in {

    val generator = oneDayMarketFactors(new LocalDate(2014, 3, 17), portfolio)

    log.info(s"Generated factors:")
    val process = generator.factors.take(10).map {
      factors => logPrices(factors)
    }
    process.run.run
  }

  private def logPrices(factors: MarketFactors) {
    val AAPL_price = factors(Price(AAPL))
    val AMZN_price = factors(Price(AMZN))
    val GS_price = factors(Price(GS))

    log.info(s"($AAPL, $AMZN, $GS) = ($AAPL_price, $AMZN_price, $GS_price)")
  }
}
