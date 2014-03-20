package kkalc.pricing

import kkalc.model.Equity
import kkalc.pricing.MarketFactor.Price

sealed trait MarketFactor

object MarketFactor {
  case class Price(equity: Equity) extends MarketFactor
}

trait MarketFactors {
  def apply(factor: MarketFactor): Option[BigDecimal] = factor match {
    case Price(equity) => price(equity)
  }

  protected def price(equity: Equity): Option[BigDecimal]
}