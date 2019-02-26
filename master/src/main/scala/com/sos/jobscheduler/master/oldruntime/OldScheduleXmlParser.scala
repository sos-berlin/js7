package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time._
import javax.xml.stream.XMLEventReader

object OldScheduleXmlParser{

  private implicit val StringAsDayOfWeek = As[String, DayOfWeek](
    (for (dayOfWeek <- DayOfWeek.values) yield dayOfWeek.toString.toLowerCase -> dayOfWeek).toMap)
  private implicit val StringAsZoneId = As[String, ZoneId](o => ZoneId.of(o))

  private implicit val StringAsLocalTime: As[String, LocalTime] = {
    val ParseRegex = """([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?""".r
    As {
      case ParseRegex(hours, minutes, seconds) =>
        LocalTime.of(hours.toInt, minutes.toInt, (Option(seconds) getOrElse "0").toInt)
      case o => throw new IllegalArgumentException(s"Not a local time: '$o'")
    }
  }

  def parse(xmlEventReader: XMLEventReader, defaultTimeZone: ZoneId): OldSchedule = {
    val eventReader = new ScalaXMLEventReader(xmlEventReader)
    import eventReader._

    def parsePeriodSeq(): PeriodSeq =
      PeriodSeq(parseElements[Period] { case _ =>
        parsePeriod()
      } map { _._2 })

    def parsePeriod(): Period =
      parseElement("period") {
        attributeMap.optionAs[LocalTime]("single_start") match {
          case Some(singleStart) =>
            SingleStartPeriod(singleStart)
          case None =>
            RepeatPeriod(
              begin = attributeMap.as[LocalTime]("begin", LocalTime.MIN),
              end = attributeMap.as[ExtendedLocalTime]("end", ExtendedLocalTime.EndOfDay),
              absoluteRepeat = bigDecimalToDuration(attributeMap.as[BigDecimal]("absolute_repeat")))
              //startOnce = attributeMap.as[Boolean]("start_once", default.startOnce))
        }
      }

    parseElement("run_time") {
      val timeZone = attributeMap.as[ZoneId]("time_zone", defaultTimeZone)
      if (peek.isEndElement)
        OldSchedule.empty(timeZone)
      else
      if (peek.asStartElement().getName.getLocalPart == "period")
        OldSchedule.daily(timeZone, parsePeriodSeq())
      else
        OldSchedule(
          timeZone,
          parseElement("weekdays") {
            parseEachRepeatingElement[(DayOfWeek, PeriodSeq)]("day") {
              val dayOfWeek = attributeMap.as[DayOfWeek]("day")
              dayOfWeek -> parsePeriodSeq()
            }
          } .toMap)
    }
  }
}
