package kkalc.pricing

import kkalc.model.{PutOption, CallOption, Equity}
import kkalc.pricing.MarketFactor.{RiskFreeRate, Volatility, DaysToMaturity, Price}
import org.joda.time.LocalDate
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ShouldMatchers, FlatSpec}
import kkalc.pricing.PricingError.MissingMarketFactors
import scalaz.NonEmptyList

class OptionPricerSpec extends FlatSpec with ShouldMatchers with MockFactory with PricerImplicits {

  val maturity = new LocalDate()

  val daysToMaturity = 30
  val spot = 100
  val strike = 100
  val riskFreeRate = 0.05
  val volatility = 0.25 / OptionPricer.VolatilityAnnualizationFactor

  val AAPL = Equity("AAPL")

  val Call = CallOption(AAPL, strike, maturity)
  val Put = PutOption(AAPL, strike, maturity)

  implicit def factors = {
    val factors = mock[MarketFactors]
    (factors.apply _).expects(DaysToMaturity(maturity)).returning(Some(daysToMaturity)).anyNumberOfTimes()
    (factors.apply _).expects(RiskFreeRate).returning(Some(riskFreeRate)).anyNumberOfTimes()
    (factors.apply _).expects(Price(AAPL)).returning(Some(spot)).anyNumberOfTimes()
    (factors.apply _).expects(Volatility(AAPL)).returning(Some(volatility)).anyNumberOfTimes()
    (factors.apply _).expects(*).returning(None).anyNumberOfTimes()
    (factors.apply _).expects(*).returning(None).anyNumberOfTimes()
    factors
  }

  "Option pricer" should "correctly price Call Option" in {
    val price = OptionPricer.price(Call)
    assert(price.isRight)
    price.toOption.get.toDouble should (be >= 3.062 and be <= 3.064)
  }

  it should "correctly price Put Option" in {
    val price = OptionPricer.price(Put)
    assert(price.isRight)
    price.toOption.get.toDouble should (be >= 2.651 and be <= 2.653)
  }

  it should "collect all missing factors" in {
    val underlying = Equity("NoSuchEquity")
    val price = OptionPricer.price(CallOption(underlying, 200, maturity))
    assert(price.isLeft)
    assert(price.toEither.left.get.isInstanceOf[MissingMarketFactors])

    val missingFactors = price.toEither.left.get.asInstanceOf[MissingMarketFactors]
    assert(missingFactors.factors == NonEmptyList(Price(underlying), Volatility(underlying)))
  }
}