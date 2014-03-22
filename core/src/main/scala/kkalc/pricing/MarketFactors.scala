package kkalc.pricing

import kkalc.model.{HistoricalPrice, Equity}
import kkalc.pricing.MarketFactor.{RiskFreeRate, DaysToMaturity, Volatility, Price}
import org.joda.time.LocalDate

sealed trait MarketFactor

object MarketFactor {

  case class Price(equity: Equity) extends MarketFactor

  case class Volatility(equity: Equity) extends MarketFactor

  case class DaysToMaturity(maturity: LocalDate) extends MarketFactor

  case object RiskFreeRate extends MarketFactor
}

trait MarketFactors {
  def apply(factor: MarketFactor): Option[Double] = factor match {
    case Price(equity) => price(equity)
    case Volatility(equity) => volatility(equity)
    case DaysToMaturity(maturity) => daysToMaturity(maturity)
    case RiskFreeRate => riskFreeRate
  }

  protected def price(equity: Equity): Option[Double]

  protected def volatility(equity: Equity): Option[Double]

  protected def daysToMaturity(maturity: LocalDate): Option[Double]

  protected def riskFreeRate: Option[Double]
}

trait HistoricalVolatility {

  import spire.algebra.Trig
  import spire.implicits._
  import spire.math._

  protected def volatility(prices: Vector[HistoricalPrice], calc: HistoricalPrice => Double = _.adjusted): Double = {
    val loge = implicitly[Trig[Double]].e.log()
    def ln(x: Double) = x.log() / loge
    stddev(diff(prices.map(calc)).map(ln))
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