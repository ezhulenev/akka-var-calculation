package kkalc.pricing

import kkalc.model.{Instrument, Equity, Position, Portfolio}
import kkalc.pricing.PortfolioPricingError.UnderlyingPricingErrors
import scalaz._, syntax.apply._

sealed trait PortfolioPricingError

object PortfolioPricingError {

  case class UnderlyingPricingErrors(errors: Vector[PricingError]) extends PortfolioPricingError

}

object PortfolioPricer {

  def price(portfolio: Portfolio)(implicit factors: MarketFactors): PortfolioPricingError \/ Double = {

    def martToMarket[I <: Instrument](instrument: I)(implicit pricer: Pricer[I]): PricingError \/ Double = {
      pricer.price(instrument)
    }

    // Mark all positions to market
    val mtm: NonEmptyList[PricingError \/ Double] = portfolio.positions.map {
      case Position(equity: Equity, n) => martToMarket(equity).map( _ * n)
    }

    // Sum all MTM values aggregating errors
    val sum: NonEmptyList[PricingError] \/ Double = \/.fromEither(
      mtm.map(_.validation.toValidationNel).stream.reduceLeft((l, r) => (l |@| r)(_ + _)).toEither
    )

    sum.fold(
      error   => -\/(UnderlyingPricingErrors(error.stream.toVector)),
      success => \/-(success)
    )
  }
}