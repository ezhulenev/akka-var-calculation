package kkalc.model

import org.joda.time.LocalDate

case class HistoricalPrice(date: LocalDate,
                           open: BigDecimal,
                           high: BigDecimal,
                           low: BigDecimal,
                           close: BigDecimal,
                           volume: BigDecimal,
                           adjusted: BigDecimal)