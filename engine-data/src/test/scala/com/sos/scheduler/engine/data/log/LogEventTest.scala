package com.sos.scheduler.engine.data.log

import com.sos.scheduler.engine.data.message.MessageCode
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class LogEventTest extends FreeSpec {
  "codeOption" in {
    new LogEvent(SchedulerLogLevel.info, "").codeOption shouldBe None
    new LogEvent(SchedulerLogLevel.info, " ABC-123 xx").codeOption shouldBe None
    new LogEvent(SchedulerLogLevel.info, "ABC-123 ").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123 xx").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123  xx").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-X123-Y123 ").codeOption shouldEqual Some(MessageCode("ABC-X123-Y123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123 x").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123 xx XXX-999 yy").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123  Error").codeOption shouldEqual Some(MessageCode("ABC-123"))
    new LogEvent(SchedulerLogLevel.info, "ABC-123  Error\nxx").codeOption shouldEqual Some(MessageCode("ABC-123"))
  }
}
