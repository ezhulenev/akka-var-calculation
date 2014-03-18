package kkalc.service

import kkalc.model.{HistoricalPrice, Equity}
import org.joda.time.LocalDate

trait MarketDataServiceModule {

  def marketDataService: MarketDataService

  trait MarketDataService {
    def historicalPrices(equity: Equity, from: LocalDate, to: LocalDate): Vector[HistoricalPrice]
  }
}