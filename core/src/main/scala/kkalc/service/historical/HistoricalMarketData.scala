package kkalc.service.historical

import kkalc.model.{HistoricalPrice, Equity}
import kkalc.service.MarketDataModule
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.slf4j.LoggerFactory
import scala.util.{Success, Failure, Try}
import scalaz.{\/, \/-, -\/}


trait HistoricalMarketData extends MarketDataModule {

  protected object marketData extends MarketData {
    private val log = LoggerFactory.getLogger(classOf[MarketData])

    private val DateFormat = DateTimeFormat.forPattern("YYYY-MM-dd")

    private def tryLoad(ticker: String) = Try {
      val is = this.getClass.getResourceAsStream(s"/price/$ticker.csv")
      assume(is != null, s"Price history for '$ticker' is unavailable")

      val lines = scala.io.Source.fromInputStream(is).getLines().drop(1)
      val split = lines.map(_.split(",").toSeq)

      val prices = split map {
        case line if line.length == 7 => HistoricalPrice(
          DateFormat.parseLocalDate(line(0).drop(1).dropRight(1)),
          line(1).toDouble,
          line(2).toDouble,
          line(3).toDouble,
          line(4).toDouble,
          line(5).toDouble,
          line(6).toDouble)


        case s => sys.error(s"Can't parse price line '$s'")
      }

      prices.toVector
    }

    def historicalPrices(equity: Equity, from: LocalDate, to: LocalDate) = {
      log.debug(s"Get equity historical prices for '${equity.ticker}' from '$from' to '$to'")

      def inRange(price: HistoricalPrice): Boolean = {
        !price.date.isBefore(from) && !price.date.isAfter(to)
      }

      tryLoad(equity.ticker) match {
        case Success(prices) => \/-(prices.filter(inRange))
        case Failure(err) => -\/(MarketDataUnavailable(err))
      }
    }

    def historicalPrice(equity: Equity, date: LocalDate): MarketDataError \/ Option[HistoricalPrice] = {
      historicalPrices(equity, date, date).map(_.headOption)
    }
  }
}