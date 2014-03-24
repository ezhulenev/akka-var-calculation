package kkalc.pricing

import kkalc.model.{Portfolio, Position, Equity}
import kkalc.pricing.MarketFactor.Price
import kkalc.pricing.PortfolioPricingError.UnderlyingPricingErrors
import kkalc.pricing.PricingError.MissingMarketFactors
import kkalc.service.historical.{HistoricalMarketFactors, HistoricalMarketData}
import org.joda.time.LocalDate
import org.scalatest.FlatSpec
import scalaz.NonEmptyList

class PortfolioPricerSpec extends FlatSpec with HistoricalMarketFactors with HistoricalMarketData {

  val AAPL = Equity("AAPL")
  val Apple = Position(AAPL, 10)
  
  val AMZN = Equity("AMZN")
  val Amazon = Position(AMZN, 30)

  // Valuation date
  val date = new LocalDate(2007, 1, 3)

  implicit val factors = marketFactors(date)(MarketFactorsParameters())

  "Portfolio pricer" should "price portfolio with one position" in {
    val portfolio = Portfolio(NonEmptyList(Apple))

    val price = PortfolioPricer.price(portfolio)
    assert(price.isRight)
    assert(price.toOption.get == (80.54 * 10))
  }

  it should "price portfolio with two positions" in {
    val portfolio = Portfolio(NonEmptyList(Apple, Amazon))

    val price = PortfolioPricer.price(portfolio)
    assert(price.isRight)
    assert(price.toOption.get == 80.54 * 10 + 38.7 * 30)
  }

  it should "fail to price portfolio with missing factor" in {
    val NO_TICKER = Equity("NO_TICKER")
    val BadCompany = Position(NO_TICKER, 30)

    val portfolio = Portfolio(NonEmptyList(Apple, Amazon, BadCompany))

    val price = PortfolioPricer.price(portfolio)
    assert(price.isLeft)

    val error = price.toEither.left.get
    assert(error.isInstanceOf[UnderlyingPricingErrors])

    val underlyingErrors = error.asInstanceOf[UnderlyingPricingErrors].errors
    assert(underlyingErrors.length == 1)
    assert(underlyingErrors.head == MissingMarketFactors(NonEmptyList(Price(NO_TICKER))))
  }
}