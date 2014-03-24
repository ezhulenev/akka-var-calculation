package kkalc.service.historical

import kkalc.model.{HistoricalPrice, EquityOption, Equity, Portfolio}
import kkalc.pricing.{HistoricalVolatility, MarketFactors}
import kkalc.service.{MarketDataModule, MarketFactorsModule}
import org.apache.commons.math.random.{CorrelatedRandomVectorGenerator, GaussianRandomGenerator, JDKRandomGenerator}
import org.apache.commons.math.stat.correlation.Covariance
import org.joda.time.{Days, LocalDate}
import scalaz.NonEmptyList
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.io.resource
import org.slf4j.LoggerFactory

trait HistoricalMarketFactors extends MarketFactorsModule with MarketDataModule with HistoricalVolatility { module =>

  class OneDayMarketFactorsGenerator(date: LocalDate, rate: Double, equities: NonEmptyList[Equity],
                                     price: Map[Equity, Double], vol: Map[Equity, Double],
                                     covariance: Covariance) extends MarketFactorsGenerator {

    private val nextDay = date.plusDays(1)

    // Generate correlated vectors
    private def generator = {
      val rg = new JDKRandomGenerator
      val gaussian = new GaussianRandomGenerator(rg)
      val covarianceMatrix = covariance.getCovarianceMatrix

      new CorrelatedRandomVectorGenerator(covarianceMatrix, 1.0e-12 * covarianceMatrix.getNorm, gaussian)
    }

    case class GeneratedMarketFactors(generatedPrice: Map[Equity, Double]) extends MarketFactors {
      override protected def riskFreeRate = Some(rate)
      override protected def price(equity: Equity) = generatedPrice.get(equity)
      override protected def volatility(equity: Equity) = vol.get(equity)
      override protected def daysToMaturity(maturity: LocalDate) =
        if (nextDay.isBefore(maturity)) Some(Days.daysBetween(nextDay, maturity).getDays) else None
    }

    override def factors: Process[Task, MarketFactors] = resource(Task.delay(generator))(
      _ => Task.delay(())) {
      gen => Task {
        val correlated = equities.stream zip gen.nextVector().map(v => v)
        val generatedPrice: Map[Equity, Double] = correlated.toSeq.map({
          case (equity, randomValue) => (equity, price(equity) + randomValue * vol(equity))
        })(scala.collection.breakOut)

        GeneratedMarketFactors(generatedPrice)
      }
    }
  }

  override protected def oneDayMarketFactors(portfolio: Portfolio, date: LocalDate)
                                  (implicit parameters: MarketFactorsParameters): MarketFactorsGenerator = {

    // Get equities
    val equities = portfolio.positions.map(_.instrument).map {
      case e: Equity => e
      case o: EquityOption => o.underlying
    }.sortBy(_.ticker)


    // Get prices at given date
    val price: Map[Equity, Double] =
      equities.
        map(equity => marketData.historicalPrice(equity, date).
        fold(
          err => sys.error(s"Market data for $equity is unavailable, error = $err"),
          priceO => priceO.map(price => (equity, price.adjusted)) getOrElse sys.error(s"Price for $equity at $date is not defined"))
        ).stream.toMap

    // Get prices with defined horizon
    val historicalPrices: Map[Equity, Vector[HistoricalPrice]] =
      equities.
        map(equity => marketData.historicalPrices(equity, date.minusDays(parameters.horizon), date).
        fold(
          err => sys.error(s"Market data for $equity is unavailable, error = $err"),
          prices => (equity, prices)
        )).stream.toMap

    // Calculate historical volatility
    val vol: Map[Equity, Double] = historicalPrices.mapValues(prices => volatility(prices))

    // and covariance matrix
    val adjustedPrices = equities.map(equity => historicalPrices(equity).map(_.adjusted).toArray).stream.toArray
    val covariance = new Covariance(adjustedPrices)

    new OneDayMarketFactorsGenerator(date, parameters.riskFreeRate, equities, price, vol, covariance)
  }

  override protected def marketFactors(date: LocalDate)(implicit parameters: MarketFactorsParameters) = new MarketFactors {
    private val log = LoggerFactory.getLogger(classOf[MarketFactors])

    log.debug(s"Construct historical market factors. Date = $date, volatility horizon = ${parameters.horizon} days")

    override protected def price(equity: Equity): Option[Double] = {
      log.debug(s"Get price for $equity at $date")
      marketData.historicalPrice(equity, date).fold(_ => None, _.map(_.adjusted))
    }

    override protected def volatility(equity: Equity): Option[Double] = {
      log.debug(s"Get volatility for $equity")
      val prices = marketData.
        historicalPrices(equity, date.minusDays(parameters.horizon), date).
        fold(_ => None, p => Some(p))

      prices.map(p => module.volatility(p))
    }

    override protected def daysToMaturity(maturity: LocalDate): Option[Double] = {
      if (date.isBefore(maturity)) {
        Some(Days.daysBetween(date, maturity).getDays)

      } else None
    }

    protected def riskFreeRate: Option[Double] = Some(parameters.riskFreeRate)
  }
}