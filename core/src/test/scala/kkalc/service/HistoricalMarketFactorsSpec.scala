package kkalc.service

import kkalc.model.Equity
import kkalc.pricing.MarketFactor.{Volatility, Price}
import kkalc.service.historical.{HistoricalMarketFactors, HistoricalMarketData}
import org.joda.time.LocalDate
import org.scalatest.{ShouldMatchers, FlatSpec}

class HistoricalMarketFactorsSpec extends FlatSpec with ShouldMatchers with HistoricalMarketFactors with HistoricalMarketData { test =>

  val AAPL = Equity("AAPL")

  implicit val params = MarketFactorsParameters()
  
  "Historical Market Factors" should "give access to historical price data" in {
    val marketFactors = test.marketFactors(new LocalDate(2007, 1, 3))

    val priceFactor = marketFactors(Price(AAPL))
    assert(priceFactor == Some(80.54))
  }
  
  it should "give access to volatility" in {
    val marketFactors = test.marketFactors(new LocalDate(2009, 1, 3))

    val volatilityFactor = marketFactors(Volatility(AAPL))
    assert(volatilityFactor.isDefined)
    volatilityFactor.get should (be >= 0.031 and be <= 0.032)
  }
}
