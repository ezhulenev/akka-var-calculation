package kkalc.pricing

import kkalc.model.{Equity, Instrument}
import kkalc.pricing.MarketFactor.Price
import kkalc.pricing.PricingError.MissingMarketFactor
import scala.annotation.implicitNotFound
import scalaz.{-\/, \/-, \/}

sealed trait PricingError

object PricingError {
  case class MissingMarketFactor(factor: MarketFactor) extends PricingError
}

/**
 * Price instrument given market factors affecting it's price
 */
@implicitNotFound(msg = "Can't find pricer for instrument type '${I}'")
trait Pricer[I <: Instrument] {
  def price(instrument: I)(implicit factors: MarketFactors): PricingError \/ BigDecimal
}


object Pricer extends PricerImplicits

trait PricerImplicits {

  // Get price value from market factors
  implicit object EquityPricer extends Pricer[Equity] {
    def price(equity: Equity)(implicit factors: MarketFactors): PricingError \/ BigDecimal = {
      val priceFactor = Price(equity)
      factors(priceFactor) map (v => \/-(v)) getOrElse -\/(MissingMarketFactor(priceFactor))
    }
  }

}