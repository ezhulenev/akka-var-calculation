package kkalc

import org.scalatest.FlatSpec
import kkalc.service.impl.MarketDataModuleImpl
import org.joda.time.LocalDate
import kkalc.model.Equity
import kkalc.pricing.MarketFactor.Price

class HistoricalMarketFactorsSpec extends FlatSpec {

  "Historical Market Factors" should "give access to historical price data" in {
    val Apple = Equity("AAPL")
    val marketFactors = new HistoricalMarketFactors(new LocalDate(2007, 1, 3)) with MarketDataModuleImpl

    val priceFactor = marketFactors(Price(Apple))
    assert(priceFactor == Some(BigDecimal("80.54")))
  }
}
