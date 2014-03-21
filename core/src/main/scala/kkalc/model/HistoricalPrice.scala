package kkalc.model

import org.joda.time.LocalDate

case class HistoricalPrice(date: LocalDate,
                           open: Double,
                           high: Double,
                           low: Double,
                           close: Double,
                           volume: Double,
                           adjusted: Double)