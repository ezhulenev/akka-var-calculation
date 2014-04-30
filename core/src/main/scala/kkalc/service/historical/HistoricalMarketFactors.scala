package kkalc.service.historical

import kkalc.model.{HistoricalPrice, EquityOption, Equity, Portfolio}
import kkalc.pricing.{OneDayMarketFactorsGenerator, MarketFactorsGenerator, HistoricalVolatility, MarketFactors}
import kkalc.service.{MarketDataModule, MarketFactorsModule}

import org.joda.time.{Days, LocalDate}
import org.slf4j.LoggerFactory
import kkalc.pricing.OneDayMarketFactorsGenerator.CurrentFactors

trait HistoricalMarketFactors extends MarketFactorsModule with MarketDataModule with HistoricalVolatility { module =>

  protected def oneDayMarketFactors(portfolio: Portfolio, date: LocalDate)
                                  (implicit parameters: MarketFactorsParameters): MarketFactorsGenerator = {

    // Collect equities & underlying equities
    val equities = portfolio.positions.map(_.instrument).map {
      case e: Equity => e
      case o: EquityOption => o.underlying
    }.sortBy(_.ticker)

    val currentFactors: Map[Equity, CurrentFactors] = equities.list.map(equity => {

      def error(err: MarketDataError) = sys.error(s"Market data for $equity is unavailable. Error: $err")
      def adjustedPrice(h: Option[HistoricalPrice]) = h.map(_.adjusted) getOrElse sys.error(s"Price for $equity at $date is not defined")

      val price = marketData.historicalPrice(equity, date).fold(error, adjustedPrice)
      val priceHistory = marketData.historicalPrices(equity, date.minusDays(parameters.horizon), date).fold(error, identity)
      val vol = volatility(priceHistory)

      equity -> CurrentFactors(price, vol, priceHistory.map(_.adjusted))

    })(scala.collection.breakOut)

    new OneDayMarketFactorsGenerator(date, parameters.riskFreeRate, currentFactors)
  }

  protected def marketFactors(date: LocalDate)(implicit parameters: MarketFactorsParameters) = new MarketFactors {
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