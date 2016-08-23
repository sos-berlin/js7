package com.sos.scheduler.engine.data.log

import com.sos.scheduler.engine.data.event.KeyedEvent.KeyedSubtype
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import com.sos.scheduler.engine.data.message.MessageCode
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class LogEventTest extends FreeSpec {

  "codeOption" in {
    LogEvent(SchedulerLogLevel.info, "").codeOption shouldBe None
    LogEvent(SchedulerLogLevel.info, " ABC-123 xx").codeOption shouldBe None
    LogEvent(SchedulerLogLevel.info, "ABC-123 ").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123 xx").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123  xx").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-X123-Y123 ").codeOption shouldEqual Some(MessageCode("ABC-X123-Y123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123 x").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123 xx XXX-999 yy").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123  Error").codeOption shouldEqual Some(MessageCode("ABC-123"))
    LogEvent(SchedulerLogLevel.info, "ABC-123  Error\nxx").codeOption shouldEqual Some(MessageCode("ABC-123"))
  }

  "JSON InfoLogEvent" in {
    checkJson(KeyedEvent(InfoLogEvent("MESSAGE-1 text")),
      """{
        "TYPE": "Logged",
        "level": "info",
        "message": "MESSAGE-1 text"
      }""")
  }

  "JSON WarningLogEvent" in {
    checkJson(KeyedEvent(WarningLogEvent("MESSAGE-1 text")),
      """{
        "TYPE": "Logged",
        "level": "warning",
        "message": "MESSAGE-1 text"
      }""")
  }

  "JSON ErrorLogEvent" in {
    checkJson(KeyedEvent(ErrorLogEvent("MESSAGE-1 text")),
      """{
        "TYPE": "Logged",
        "level": "error",
        "message": "MESSAGE-1 text"
      }""")
  }

  "JSON OtherLogEvent" in {
    checkJson(KeyedEvent(LogEvent(SchedulerLogLevel.debug3, "MESSAGE-1 text")),
      """{
        "TYPE": "Logged",
        "level": "debug3",
        "message": "MESSAGE-1 text"
      }""")
  }

  private def checkJson(event: AnyKeyedEvent, json: String): Unit = {
    implicit val jsonFormat = KeyedEvent.typedJsonFormat[Event](
      KeyedSubtype[LogEvent])
    assert(jsonFormat canSerialize event)
    val jsValue = json.parseJson
    assert (event.toJson == jsValue)
    assert (event == jsValue.convertTo[AnyKeyedEvent] )
  }
}
