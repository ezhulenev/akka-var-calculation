package kkalc.service

import kkalc.model.Portfolio
import org.joda.time.LocalDate

trait MarketRiskCalculator {

  type MarketRisk <: MarketRiskLike

  trait MarketRiskLike {
    def VaR(p: Double): Double
    def conditionalVaR(p: Double): Double
  }

  def marketRisk(portfolio: Portfolio, date: LocalDate): MarketRisk
}