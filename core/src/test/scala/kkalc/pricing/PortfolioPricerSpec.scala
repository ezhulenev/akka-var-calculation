package kkalc.pricing

import kkalc.HistoricalMarketFactors
import kkalc.model.{Portfolio, Position, Equity}
import kkalc.pricing.MarketFactor.Price
import kkalc.pricing.PortfolioPricingError.UnderlyingPricingErrors
import kkalc.service.impl.MarketDataModuleImpl
import org.joda.time.LocalDate
import org.scalatest.FlatSpec

class PortfolioPricerSpec extends FlatSpec {

  val AAPL = Equity("AAPL")
  val Apple = Position(AAPL, 10)
  
  val AMZN = Equity("AMZN")
  val Amazon = Position(AMZN, 30)

  // Valuation date
  val date = new LocalDate(2007, 1, 3)

  implicit val factors = new HistoricalMarketFactors(date) with MarketDataModuleImpl

  "Portfolio pricer" should "price portfolio with one position" in {
    val portfolio = Portfolio(Vector(Apple))

    val price = PortfolioPricer.price(portfolio)
    assert(price.isRight)
    assert(price.toOption.get == (BigDecimal("80.54") * 10))
  }

  it should "price portfolio with two positions" in {
    val portfolio = Portfolio(Vector(Apple, Amazon))

    val price = PortfolioPricer.price(portfolio)
    assert(price.isRight)
    assert(price.toOption.get == BigDecimal("80.54") * 10 + BigDecimal("38.7") * 30)
  }

  it should "fail to price portfolio with missing factor" in {
    val NO_TICKER = Equity("NO_TICKER")
    val BadCompany = Position(NO_TICKER, 30)

    val portfolio = Portfolio(Vector(Apple, Amazon, BadCompany))

    val price = PortfolioPricer.price(portfolio)
    assert(price.isLeft)

    val error = price.toEither.left.get
    assert(error.isInstanceOf[UnderlyingPricingErrors])

    val underlyingErrors = error.asInstanceOf[UnderlyingPricingErrors].errors
    assert(underlyingErrors.length == 1)
    assert(underlyingErrors.head == PricingError.MissingMarketFactor(Price(NO_TICKER)))
  }
}