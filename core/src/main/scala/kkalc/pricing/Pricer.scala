package kkalc.pricing

import kkalc.model._
import kkalc.pricing.MarketFactor.DaysToMaturity
import kkalc.pricing.MarketFactor.Price
import kkalc.pricing.MarketFactor.RiskFreeRate
import kkalc.pricing.MarketFactor.Volatility
import kkalc.pricing.PricingError.MissingMarketFactors
import org.apache.commons.math.distribution.NormalDistributionImpl
import org.joda.time.LocalDate
import scala.annotation.implicitNotFound
import scalaz.NonEmptyList.nels
import scalaz.{\/, NonEmptyList, -\/, \/-, ValidationNel}

sealed trait PricingError

object PricingError {
  case class MissingMarketFactors(factors: NonEmptyList[MarketFactor]) extends PricingError
}

/**
 * Price instrument given market factors affecting it's price
 */
@implicitNotFound(msg = "Can't find pricer for instrument type '${I}'")
trait Pricer[I <: Instrument] {
  def price(instrument: I)(implicit factors: MarketFactors): PricingError \/ Double
}


object Pricer extends PricerImplicits

trait PricerImplicits {

  // Get price value from market factors
  implicit object EquityPricer extends Pricer[Equity] {
    def price(equity: Equity)(implicit factors: MarketFactors): PricingError \/ Double = {
      val priceFactor = Price(equity)
      factors(priceFactor) map (v => \/-(v)) getOrElse -\/(MissingMarketFactors(nels(priceFactor)))
    }
  }

  /**
   * Use Black-Scholes formula to price Option
   * http://en.wikipedia.org/wiki/Black–Scholes_model
   */
  implicit object OptionPricer extends Pricer[EquityOption] {

    import spire.algebra.Trig
    import spire.implicits._
    import spire.math._

    private[pricing] case class Parameters(daysToMaturity: Int,
                                  spot: Double,
                                  strike: Double,
                                  rate: Double,
                                  sigma: Double) {
      def timeToMaturity = daysToMaturity / 365.0
    }
    
    override def price(option: EquityOption)(implicit factors: MarketFactors): PricingError \/ Double = {

      def factor(factor: MarketFactor): MissingMarketFactors \/ Double = {
        factors(factor).map(v => \/-(v)) getOrElse -\/(MissingMarketFactors(nels(factor)))
      }

      def parameters(equity: Equity, strike: Double, maturity: LocalDate): PricingError \/ Parameters = {
        import scalaz.syntax.applicative._


        val daysToMaturity = factor(DaysToMaturity(maturity)).validation.toValidationNel
        val spot = factor(Price(equity)).validation.toValidationNel
        val rate = factor(RiskFreeRate).validation.toValidationNel
        val sigma = factor(Volatility(equity)).validation.toValidationNel

        val parameters: ValidationNel[MissingMarketFactors, Parameters] = (daysToMaturity |@| spot |@| rate |@| sigma) {
          case (days, sp, rt, si) => Parameters(days.toInt(), sp, strike, rt, si)
        }

        parameters.fold(missingFactors => -\/(MissingMarketFactors(missingFactors.flatMap(_.factors))), p => \/-(p))
      }

      option match {
        case CallOption(equity, strike, maturity) => parameters(equity, strike, maturity).map(call)
        case PutOption(equity, strike, maturity)  => parameters(equity, strike, maturity).map(put)
      }
    }

    private[this] val normalDistribution = new NormalDistributionImpl(0, 1)
    private[this] val loge = implicitly[Trig[Double]].e.log()
    private[this] def ln(x: Double) = x.log() / loge

    private[this] def d1(parameters: Parameters): Double = {
      import parameters._

      val l = 1 / (sigma * sqrt(timeToMaturity))
      val r = ln(spot / strike) + (rate + (sigma pow  2) / 2) * timeToMaturity

      l * r
    }

    private[this] def d2(parameters: Parameters): Double = {
      import parameters._

      val l = 1 / (sigma * sqrt(timeToMaturity))
      val r = ln(spot / strike) + (rate - (sigma pow 2) / 2) * timeToMaturity

      l * r
    }

    private[this] def N(v: Double) = normalDistribution.cumulativeProbability(v.toDouble)

    private[pricing] def call(parameters: Parameters): Double = {
      import parameters._

      val D1 = d1(parameters)
      val D2 = d2(parameters)

      N(D1) * spot - N(D2) * strike * exp(-1 * rate * timeToMaturity)
    }

    private[pricing] def put(parameters: Parameters): Double = {
      import parameters._

      val D1 = d1(parameters)
      val D2 = d2(parameters)

      N(-1 * D2) * strike * exp(-1 * rate * timeToMaturity) - N(-1 * D1) * spot
    }
  }
}