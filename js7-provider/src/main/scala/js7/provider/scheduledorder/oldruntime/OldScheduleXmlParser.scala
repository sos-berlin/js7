package js7.provider.scheduledorder.oldruntime

import java.time.{DayOfWeek, LocalTime, ZoneId}
import javax.xml.stream.XMLEventReader
import js7.base.convert.As
import js7.base.time.JavaTime.*
import js7.common.scalautil.xmls.ScalaXMLEventReader

object OldScheduleXmlParser:

  private implicit val StringAsDayOfWeek: As[String, DayOfWeek] =
    As(
      (for dayOfWeek <- DayOfWeek.values yield dayOfWeek.toString.toLowerCase -> dayOfWeek).toMap)

  private implicit val StringAsZoneId: As[String, ZoneId] =
    As(o => ZoneId.of(o))

  private implicit val StringAsLocalTime: As[String, LocalTime] =
    val ParseRegex = """([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?""".r
    As:
      case ParseRegex(hours, minutes, seconds) =>
        LocalTime.of(hours.toInt, minutes.toInt, Option(seconds).getOrElse("0").toInt)
      case o => throw new IllegalArgumentException(s"Not a local time: '$o'")

  def parse(xmlEventReader: XMLEventReader, defaultTimeZone: ZoneId): OldSchedule =
    val eventReader = new ScalaXMLEventReader(xmlEventReader)
    import eventReader.*

    def parsePeriodSeq(): PeriodSeq =
      PeriodSeq(parseElements[Period] { case _ =>
        parsePeriod()
      }.map(_._2))

    def parsePeriod(): Period =
      parseElement("period"):
        attributeMap.optionAs[LocalTime]("single_start") match
          case Some(singleStart) =>
            SingleStartPeriod(singleStart)
          case None =>
            RepeatPeriod(
              begin = attributeMap.as[LocalTime]("begin", LocalTime.MIN),
              end = attributeMap.as[ExtendedLocalTime]("end", ExtendedLocalTime.EndOfDay),
              absoluteRepeat = bigDecimalToDuration(attributeMap.as[BigDecimal]("absolute_repeat")))
              //startOnce = attributeMap.as[Boolean]("start_once", default.startOnce))

    parseElement("run_time"):
      val timeZone = attributeMap.as[ZoneId]("time_zone", defaultTimeZone)
      if peek.isEndElement then
        OldSchedule.empty(timeZone)
      else
      if peek.asStartElement().getName.getLocalPart == "period" then
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
