package kkalc.pricing

import kkalc.model.Equity

import org.apache.commons.math.random.{CorrelatedRandomVectorGenerator, GaussianRandomGenerator, JDKRandomGenerator}
import org.apache.commons.math.stat.correlation.Covariance
import org.joda.time.{Days, LocalDate}

import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.io._
import kkalc.pricing.OneDayMarketFactorsGenerator.CurrentFactors

trait MarketFactorsGenerator {
  def factors: Process[Task, MarketFactors]
}

/**
 * Generate one-day market factors forecast base on given 'current' market factors
 */

object OneDayMarketFactorsGenerator {
  case class CurrentFactors(price: Double, vol: Double, priceHistory: Vector[Double])
}

class OneDayMarketFactorsGenerator(date: LocalDate, rate: Double, currentFactors: Map[Equity, CurrentFactors]) extends MarketFactorsGenerator with Serializable {

  private val nextDay = date.plusDays(1)

  // Generate correlated vectors
  private def generator = {
    val rg = new JDKRandomGenerator
    val gaussian = new GaussianRandomGenerator(rg)
    val priceHistory = currentFactors.values.map(_.priceHistory.toArray).toArray
    val covariance = new Covariance(priceHistory)
    val covarianceMatrix = covariance.getCovarianceMatrix

    new CorrelatedRandomVectorGenerator(covarianceMatrix, 1.0e-12 * covarianceMatrix.getNorm, gaussian)
  }

  case class GeneratedMarketFactors(generatedPrice: Map[Equity, Double]) extends MarketFactors {
    protected def riskFreeRate = Some(rate)
    protected def price(equity: Equity) = generatedPrice.get(equity)
    protected def volatility(equity: Equity) = currentFactors.get(equity).map(_.vol)
    protected def daysToMaturity(maturity: LocalDate) =
      if (nextDay.isBefore(maturity)) Some(Days.daysBetween(nextDay, maturity).getDays) else None
  }

  def factors: Process[Task, MarketFactors] = resource(Task.delay(generator))(
    _ => Task.delay(())) {
    gen => Task.delay {
      val correlated = currentFactors.keys zip gen.nextVector().map(v => v)
      val generatedPrice: Map[Equity, Double] = correlated.toSeq.map({
        case (equity, randomValue) => (equity, currentFactors(equity).price + randomValue * currentFactors(equity).vol)
      })(scala.collection.breakOut)

      GeneratedMarketFactors(generatedPrice)
    }
  }
}