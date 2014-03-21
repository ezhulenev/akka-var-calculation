package kkalc.model

import org.joda.time.LocalDate

sealed trait Instrument

case class Equity(ticker: String) extends Instrument

sealed trait EquityOption extends Instrument {
  def underlying: Equity
}

case class CallOption(underlying: Equity, strike: Double, maturity: LocalDate) extends EquityOption

case class PutOption(underlying: Equity, strike: Double, maturity: LocalDate) extends EquityOption