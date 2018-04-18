package com.sos.jobscheduler.master.scheduledorder

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.xmls.XmlSources._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.oldruntime.OldSchedule.EveryDay
import com.sos.jobscheduler.master.oldruntime.{OldSchedule, PeriodSeq, RepeatPeriod}
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorXmlParserTest extends FreeSpec {

  "parse" in {
    val id = ScheduledOrderGeneratorPath("/TEST") % "VERSION"
    val timeZone = ZoneId.of("Europe/Berlin")
    val orderGenerator = ScheduledOrderGeneratorXmlParser.parseXml(id,
      <order job_chain="/JOBCHAIN">
        <params>
          <param name="a" value="AAA"/>
        </params>
        <run_time>
          <period absolute_repeat="10"/>
        </run_time>
      </order>,
      timeZone).orThrow
    assert(orderGenerator == ScheduledOrderGenerator(
      id,
      WorkflowPath("/JOBCHAIN"),
      Map("a" → "AAA"),
      OldSchedule(timeZone, EveryDay(PeriodSeq(List(RepeatPeriod.wholeDay(10.s)))))))
  }
}
