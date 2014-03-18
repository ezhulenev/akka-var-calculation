package kkalc.model

case class Position(instrument: Instrument, n: Int)

case class Portfolio(positions: Vector[Position])