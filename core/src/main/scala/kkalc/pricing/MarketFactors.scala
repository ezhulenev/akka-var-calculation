package kkalc.pricing

import kkalc.model.{HistoricalPrice, Equity}
import kkalc.pricing.MarketFactor.{Volatility, Price}

sealed trait MarketFactor

object MarketFactor {

  case class Price(equity: Equity) extends MarketFactor

  case class Volatility(equity: Equity) extends MarketFactor

}

trait MarketFactors {
  def apply(factor: MarketFactor): Option[BigDecimal] = factor match {
    case Price(equity) => price(equity)
    case Volatility(equity) => volatility(equity)
  }

  protected def price(equity: Equity): Option[BigDecimal]

  protected def volatility(equity: Equity): Option[BigDecimal]

}

trait VolatilityOps {

  import spire.algebra.Trig
  import spire.implicits._
  import spire.math._

  private[this] val VolatilityAnnualizationFactor = BigDecimal(Math.sqrt(260))

  protected def volatility(prices: Vector[HistoricalPrice], calc: HistoricalPrice => BigDecimal = _.adjusted): BigDecimal = {
    stddev(diff(prices.map(calc))) * VolatilityAnnualizationFactor
  }

  private[this] def mean[A: Numeric](xs: Vector[A]): A = {
    xs.reduceOption(_ + _).map(_ / xs.size) getOrElse implicitly[Numeric[A]].zero
  }

  private[this] def stddev[A: Numeric](xs: Vector[A]): A = {
    val mu = mean(xs)
    xs.map(_ - mu).
      map(_.pow(2)).
      reduceOption(_ + _).
      map(_ / xs.size).
      map(_.sqrt()) getOrElse implicitly[Numeric[A]].zero
  }

  private[this] def diff[A: Numeric : Trig](xs: Vector[A]): Vector[A] = {
    val pl = xs.dropRight(1)
    val pr = xs.drop(1)

    (pl zip pr).map {
      case (l, r) => r / l
    }
  }
}