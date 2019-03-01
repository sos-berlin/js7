package com.sos.jobscheduler.provider.scheduledorder.oldruntime

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.DayOfWeek._
import java.time.{LocalTime, ZoneId}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OldScheduleXmlParserTest extends FreeSpec {

  private val timeZone = ZoneId.of("Europe/Berlin")

  "Empty" in {
    val x = <run_time/>
    assert(parse(x) == OldSchedule.empty(timeZone))
  }

  "Simple" in {
    val x =
      <run_time>
        <period absolute_repeat="10"/>
      </run_time>
    assert(parse(x) == OldSchedule.daily(timeZone, RepeatPeriod.wholeDay(10.s)))
  }

  "RepeatPeriod" in {
    val x =
      <run_time>
        <period begin="19:00" end="22:00" absolute_repeat="3"/>
        <period begin="02:00" end="07:00" absolute_repeat="1"/>
        <period begin="09:00" end="17:00" absolute_repeat="2"/>
      </run_time>
    val periodSeq = PeriodSeq(List(
      RepeatPeriod(LocalTime.of( 2, 0), ExtendedLocalTime.of( 7, 0), 1.s),
      RepeatPeriod(LocalTime.of( 9, 0), ExtendedLocalTime.of(17, 0), 2.s),
      RepeatPeriod(LocalTime.of(19, 0), ExtendedLocalTime.of(22, 0), 3.s)))
    assert(parse(x) == OldSchedule.daily(timeZone, periodSeq))
  }

  "weekdays" in {
    val x =
      <run_time>
        <weekdays>
          <day day="monday">
            <period single_start="01:01"/>
            <period begin="01:01" end="01:01" absolute_repeat="1"/>
          </day>
          <day day="tuesday">
            <period begin="02:02" end="02:22" absolute_repeat="2"/>
            <period begin="22:02" end="22:22" absolute_repeat="22"/>
          </day>
          <day day="wednesday">
            <period single_start="03:03"/>
          </day>
          <day day="thursday">
            <period single_start="04:04"/>
          </day>
          <day day="friday">
            <period single_start="05:05"/>
          </day>
          <day day="saturday">
            <period single_start="06:06"/>
          </day>
          <day day="sunday">
            <period single_start="07:07"/>
          </day>
        </weekdays>
      </run_time>
    assert(parse(x) == OldSchedule(timeZone, Map(
      MONDAY -> PeriodSeq(List(
        SingleStartPeriod(LocalTime.of(1, 1)),
        RepeatPeriod(LocalTime.of(1, 1), LocalTime.of(1, 1), 1.s))),
      TUESDAY -> PeriodSeq(List(
        RepeatPeriod(LocalTime.of( 2, 2), LocalTime.of( 2, 22),  2.s),
        RepeatPeriod(LocalTime.of(22, 2), LocalTime.of(22, 22), 22.s))),
      WEDNESDAY -> PeriodSeq(List(
        SingleStartPeriod(LocalTime.of(3, 3)))),
      THURSDAY -> PeriodSeq(List(
        SingleStartPeriod(LocalTime.of(4, 4)))),
      FRIDAY -> PeriodSeq(List(
        SingleStartPeriod(LocalTime.of(5, 5)))),
      SATURDAY -> PeriodSeq(List(
        SingleStartPeriod(LocalTime.of(6, 6)))),
      SUNDAY -> PeriodSeq(List(
        SingleStartPeriod(LocalTime.of(7, 7)))))))
  }

  private def parse(x: xml.Elem) =
    ScalaXMLEventReader.parseDocument(x) { o =>
      OldScheduleXmlParser.parse(o, timeZone)
    }
}
