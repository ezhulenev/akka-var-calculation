package kkalc.service

import org.scalatest.{GivenWhenThen, FlatSpec}
import kkalc.service.impl.MarketDataModuleImpl
import kkalc.model.Equity
import org.joda.time.LocalDate

class MarketDataSpec extends FlatSpec with GivenWhenThen with MarketDataModuleImpl {

  val Apple = Equity("AAPL")
  val BadTicker = Equity("BADTICKER")

  val from = new LocalDate(2007, 1, 1)
  val to = new LocalDate(2014, 1, 1)

  "Market Data Service" should "load historical prices for 'AAPL'" in {
    Given("historical prices for Apple")
    val prices = marketData.historicalPrices(Apple, from, to)
    assert(prices.isRight)

    Then("price for should match line: '\"2007-01-03\",86.29,86.58,81.9,83.8,44225700,80.54'")
    val price = prices.toOption.get.find(_.date == new LocalDate(2007, 1, 3))
    assert(price.isDefined)
    assert(price.get.adjusted == BigDecimal("80.54"))
  }

  it should "fail to load prices for not existing ticker" in {
    val prices = marketData.historicalPrices(BadTicker, from, to)
    assume(prices.isLeft)
  }

}
