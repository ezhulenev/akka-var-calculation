package kkalc.service.impl

import kkalc.service.MarketDataModule
import kkalc.model.{HistoricalPrice, Equity}
import org.joda.time.LocalDate
import org.slf4j.LoggerFactory
import scala.util.{Success, Failure, Try}
import org.joda.time.format.DateTimeFormat
import scalaz.{\/-, -\/}


trait MarketDataModuleImpl extends MarketDataModule {

  object marketData extends MarketData {
    private val log = LoggerFactory.getLogger(classOf[MarketData])

    private val DateFormat = DateTimeFormat.forPattern("YYYY-MM-DD")

    private def tryLoad(ticker: String) = Try {
      val is = this.getClass.getResourceAsStream(s"/price/$ticker.csv")
      assume(is != null, s"Price history for '$ticker' is unavailable")

      val lines = scala.io.Source.fromInputStream(is).getLines().drop(1)
      val split = lines.map(_.split(",").toSeq)

      val prices = split map {
        case line if line.length == 7 => HistoricalPrice(
          DateFormat.parseLocalDate(line(0).drop(1).dropRight(1)),
          BigDecimal(line(1)),
          BigDecimal(line(2)),
          BigDecimal(line(3)),
          BigDecimal(line(4)),
          BigDecimal(line(5)),
          BigDecimal(line(6)))


        case s => sys.error(s"Can't parse price line '$s'")
      }

      prices.toVector
    }

    override def historicalPrices(equity: Equity, from: LocalDate, to: LocalDate) = {
      log.info(s"Get equity historical prices for '${equity.ticker}' from '$from' to '$to'")

      def inRange(price: HistoricalPrice): Boolean = {
        !price.date.isBefore(from) && !price.date.isAfter(to)
      }

      tryLoad(equity.ticker) match {
        case Success(prices) => \/-(prices.filter(inRange))
        case Failure(err) => -\/(MarketDataUnavailable(err))
      }
    }
  }
}