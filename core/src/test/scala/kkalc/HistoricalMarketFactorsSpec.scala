package kkalc

import kkalc.model.Equity
import kkalc.pricing.MarketFactor.{Volatility, Price}
import kkalc.service.impl.MarketDataModuleImpl
import org.joda.time.LocalDate
import org.scalatest.{ShouldMatchers, FlatSpec}

class HistoricalMarketFactorsSpec extends FlatSpec with ShouldMatchers {

  val AAPL = Equity("AAPL")
  
  "Historical Market Factors" should "give access to historical price data" in {
    val marketFactors = new HistoricalMarketFactors(new LocalDate(2007, 1, 3)) with MarketDataModuleImpl

    val priceFactor = marketFactors(Price(AAPL))
    assert(priceFactor == Some(BigDecimal("80.54")))
  }
  
  it should "give access to volatility" in {
    val marketFactors = new HistoricalMarketFactors(new LocalDate(2009, 1, 3), volatilityHorizon = 1000) with MarketDataModuleImpl

    val volatilityFactor = marketFactors(Volatility(AAPL))
    assert(volatilityFactor.isDefined)
    volatilityFactor.get.toDouble should (be >= 0.505 and be <= 0.506)
  }
}
