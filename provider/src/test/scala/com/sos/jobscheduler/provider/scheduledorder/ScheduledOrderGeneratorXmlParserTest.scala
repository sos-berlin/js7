package js7.provider.scheduledorder

import js7.base.problem.Checked.Ops
import js7.common.scalautil.xmls.XmlSources._
import js7.common.time.JavaTime._
import js7.data.workflow.WorkflowPath
import js7.provider.scheduledorder.oldruntime.OldSchedule.EveryDay
import js7.provider.scheduledorder.oldruntime.{OldSchedule, PeriodSeq, RepeatPeriod}
import java.time.ZoneId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ScheduledOrderGeneratorXmlParserTest extends AnyFreeSpec {

  "parse" in {
    val id = ScheduledOrderGeneratorPath("/TEST") ~ "VERSION"
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
      Map("a" -> "AAA"),
      OldSchedule(timeZone, EveryDay(PeriodSeq(List(RepeatPeriod.wholeDay(10.s)))))))
  }
}
