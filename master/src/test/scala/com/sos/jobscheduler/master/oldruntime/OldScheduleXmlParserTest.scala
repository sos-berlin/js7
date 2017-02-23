package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.{LocalTime, ZoneId}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OldScheduleXmlParserTest extends FreeSpec {

  private val timeZone = ZoneId.of("Europe/Berlin")

  "Empty" in {
    val x = <run_time/>
    val periodSeq = PeriodSeq(List(Period()))
    assert(parse(x) == OldSchedule(timeZone, periodSeq, startOnce = false))
  }

  "Simple" in {
    val x = <run_time absolute_repeat="10"/>
    assert(parse(x) == OldSchedule(timeZone, PeriodSeq(List(Period(absoluteRepeat = Some(10.s)))), startOnce = false))
  }

  "Period" in {
    val x =
      <run_time absolute_repeat="3600">
        <period begin="19:00" end="22:00"/>
        <period begin="02:00" end="07:00"/>
        <period begin="09:00" end="17:00"/>
      </run_time>
    val periodSeq = PeriodSeq(List(
      Period(
        begin = LocalTime.of(2, 0),
        end = ExtendedLocalTime.of(7, 0),
        absoluteRepeat = Some(1.h)),
      Period(
        begin = LocalTime.of(9, 0),
        end = ExtendedLocalTime.of(17, 0),
        absoluteRepeat = Some(1.h)),
      Period(
        begin = LocalTime.of(19, 0),
        end = ExtendedLocalTime.of(22, 0),
        absoluteRepeat = Some(1.h))))
    assert(parse(x) == OldSchedule(timeZone, periodSeq, startOnce = false))
  }

  private def parse(x: xml.Elem) =
    ScalaXMLEventReader.parseDocument(x) { o ⇒
      OldScheduleXmlParser.parse(o, timeZone)
    }
}
