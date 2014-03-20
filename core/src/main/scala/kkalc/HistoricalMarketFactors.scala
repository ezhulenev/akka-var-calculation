package kkalc

import org.joda.time.LocalDate
import kkalc.service.MarketDataModule
import kkalc.pricing.MarketFactors
import org.slf4j.LoggerFactory
import kkalc.model.Equity

abstract class HistoricalMarketFactors(date: LocalDate) extends MarketFactors with MarketDataModule {
  private[this] val log = LoggerFactory.getLogger(classOf[HistoricalMarketFactors])

  override protected def price(equity: Equity): Option[BigDecimal] = {
    log.debug(s"Get price for $equity at $date")
    marketData.historicalPrice(equity, date).fold(_ => None, _.map(_.adjusted))
  }
}