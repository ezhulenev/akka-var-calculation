package kkalc.pricing

import kkalc.model.Equity

import org.apache.commons.math.random.{CorrelatedRandomVectorGenerator, GaussianRandomGenerator, JDKRandomGenerator}
import org.apache.commons.math.stat.correlation.Covariance
import org.joda.time.{Days, LocalDate}

import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.io._

trait MarketFactorsGenerator {
  def factors: Process[Task, MarketFactors]
}

/**
 * Generate one-day market factors forecast base on given 'current' market factors
 */
class OneDayMarketFactorsGenerator(date: LocalDate, rate: Double, equities: Vector[Equity],
                                   price: Map[Equity, Double], vol: Map[Equity, Double],
                                   priceHistory: Array[Array[Double]]) extends MarketFactorsGenerator with Serializable {

  private val nextDay = date.plusDays(1)

  // Generate correlated vectors
  private def generator = {
    val rg = new JDKRandomGenerator
    val gaussian = new GaussianRandomGenerator(rg)
    val covariance = new Covariance(priceHistory)
    val covarianceMatrix = covariance.getCovarianceMatrix

    new CorrelatedRandomVectorGenerator(covarianceMatrix, 1.0e-12 * covarianceMatrix.getNorm, gaussian)
  }

  case class GeneratedMarketFactors(generatedPrice: Map[Equity, Double]) extends MarketFactors {
    protected def riskFreeRate = Some(rate)
    protected def price(equity: Equity) = generatedPrice.get(equity)
    protected def volatility(equity: Equity) = vol.get(equity)
    protected def daysToMaturity(maturity: LocalDate) =
      if (nextDay.isBefore(maturity)) Some(Days.daysBetween(nextDay, maturity).getDays) else None
  }

  def factors: Process[Task, MarketFactors] = resource(Task.delay(generator))(
    _ => Task.delay(())) {
    gen => Task.delay {
      val correlated = equities zip gen.nextVector().map(v => v)
      val generatedPrice: Map[Equity, Double] = correlated.toSeq.map({
        case (equity, randomValue) => (equity, price(equity) + randomValue * vol(equity))
      })(scala.collection.breakOut)

      GeneratedMarketFactors(generatedPrice)
    }
  }
}