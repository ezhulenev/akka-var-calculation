package kkalc.service

import kkalc.model.{HistoricalPrice, Equity}

import org.joda.time.LocalDate

import scalaz.\/

trait MarketDataModule {

  protected def marketData: MarketData

  sealed trait MarketDataError
  case class MarketDataUnavailable(error: Throwable) extends MarketDataError

  trait MarketData {
    def historicalPrices(equity: Equity, from: LocalDate, to: LocalDate):  MarketDataError \/ Vector[HistoricalPrice]

    def historicalPrice(equity: Equity, date: LocalDate): MarketDataError \/ Option[HistoricalPrice]
  }
}