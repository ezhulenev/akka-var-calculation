package kkalc.model

import scalaz.NonEmptyList

case class Position(instrument: Instrument, n: Int)

case class Portfolio(positions: NonEmptyList[Position])