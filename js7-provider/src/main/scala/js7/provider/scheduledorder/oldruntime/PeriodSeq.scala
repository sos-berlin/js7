package js7.provider.scheduledorder.oldruntime

import java.time.LocalTime
import js7.provider.scheduledorder.oldruntime.PeriodSeq._
import scala.language.implicitConversions

abstract sealed case class PeriodSeq(orderedSeq: Seq[Period]) {

  requireOrderedAndNotOverlapping(orderedSeq)

  def nextLocalTime(from: LocalTime): Option[LocalTime] =
    (orderedSeq flatMap { _.nextLocalTime(from) }).headOption

//  def nextInterval(from: DateTime): Option[Interval] = {
//    val date = from.toDateMidnight
//    val timeOfDay = LocalTime(from.getMillisOfDay)
//    nextPeriod(timeOfDay)  map periodToInterval(date)
//  }

  //def nextPeriod(from: LocalTime): Option[RepeatPeriod] =
  //  orderedSeq find { _ contains from }
}

object PeriodSeq {
  val Empty = PeriodSeq(Nil)

  def apply(o: Seq[Period]) =
    new PeriodSeq(o.sorted) {}

  implicit def fromPeriod(period: Period): PeriodSeq =
    PeriodSeq(List(period))

  private def requireOrderedAndNotOverlapping(orderedPeriods: Iterable[Period]): Unit = {
    if (orderedPeriods.sizeIs > 2) {
      for (Seq(a, b) <- orderedPeriods sliding 2) {
        require(a.end <= b.begin, s"Periods overlap: $a and $b")
      }
    }
  }
}
