package kkalc

import kkalc.model.Equity
import kkalc.pricing.MarketFactor.{Volatility, Price}
import kkalc.service.historical.HistoricalMarketData
import org.joda.time.LocalDate
import org.scalatest.{ShouldMatchers, FlatSpec}

class HistoricalMarketFactorsSpec extends FlatSpec with ShouldMatchers {

  val AAPL = Equity("AAPL")
  
  "Historical Market Factors" should "give access to historical price data" in {
    val marketFactors = new HistoricalMarketFactors(new LocalDate(2007, 1, 3)) with HistoricalMarketData

    val priceFactor = marketFactors(Price(AAPL))
    assert(priceFactor == Some(80.54))
  }
  
  it should "give access to volatility" in {
    val marketFactors = new HistoricalMarketFactors(new LocalDate(2009, 1, 3), volatilityHorizon = 1000) with HistoricalMarketData

    val volatilityFactor = marketFactors(Volatility(AAPL))
    assert(volatilityFactor.isDefined)
    volatilityFactor.get should (be >= 0.031 and be <= 0.032)
  }
}
