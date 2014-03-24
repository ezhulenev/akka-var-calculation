package kkalc.service.simulation

import kkalc.model.Portfolio
import kkalc.pricing.PortfolioPricer
import kkalc.service.{MarketDataModule, MarketFactorsModule, MarketRiskCalculator}
import org.joda.time.LocalDate
import scalaz.Monoid
import scalaz.concurrent.Task
import scalaz.stream._

trait PortfolioValueSimulation {
  self: MarketFactorsModule with MonteCarloMarketRiskCalculator =>

  /**
   * Scalaz-Stream Channel that for given market factors generator
   * runs defined number of simulations and produce MarketRisk
   */
  def simulation(portfolio: Portfolio, simulations: Int): Channel[Task, MarketFactorsGenerator, Simulations]
}

abstract class MonteCarloMarketRiskCalculator(simulations: Int = 1000000, splitFactor: Int = 10)
  extends MarketRiskCalculator
  with MarketDataModule
  with MarketFactorsModule
  with PortfolioValueSimulation { calculator =>

  case class Simulations(simulations: Vector[Double])

  private[this] implicit object M extends Monoid[Simulations] {
    val zero = Simulations(simulations = Vector.empty)

    def append(f1: Simulations, f2: => Simulations) =
      Simulations(simulations = f1.simulations ++ f2.simulations)
  }

  /**
   * Get market risk from Portfolio value simulations
   *
   * @param initialValue Initial Portfolio value
   * @param simulations Portfolio value simulations
   */
  class MarketRisk(initialValue: Double, simulations: Simulations) extends MarketRiskLike {

    private lazy val sortedPnL = simulations.simulations.map(_ - initialValue).sorted
    
    private def threshold(p: Double) = ((1 - p) * simulations.simulations.size).toInt

    private def mean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
      num.toDouble(ts.sum) / ts.size
    }
    
    def VaR(p: Double): Double = sortedPnL(threshold(p))

    def conditionalVaR(p: Double): Double = mean(sortedPnL.take(threshold(p)))
  }

  private implicit object Config extends MarketFactorsParameters(riskFreeRate = 0.05, horizon = 1000)

  private val P = Process

  override def marketRisk(portfolio: Portfolio, date: LocalDate): MarketRisk = {

    // Get initial Portfolio value
    implicit val initialFactors = marketFactors(date)
    val initialPortfolioValue =
      PortfolioPricer.price(portfolio).fold(err => sys.error(s"Failed price portfolio: $err"), identity)

    // Run portfolio values simulation
    val oneSimulation = simulations / splitFactor
    val simulationChannel = simulation(portfolio, oneSimulation)

    val generator = oneDayMarketFactors(portfolio, date)

    val process = P.
      range(0, splitFactor).
      map(_ => generator).
      through(simulationChannel).
      runFoldMap(identity)

    // Produce market-risk object from initial value and simulated values
    new MarketRisk(initialPortfolioValue, process.run)
  }
}