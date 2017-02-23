package com.sos.scheduler.engine.master.order

import com.sos.scheduler.engine.common.scalautil.xmls.XmlSources._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.engine2.order.{JobChainPath, NodeId, NodeKey}
import com.sos.scheduler.engine.master.oldruntime.OldSchedule.EveryDay
import com.sos.scheduler.engine.master.oldruntime.{OldSchedule, Period, PeriodSeq}
import java.time.ZoneId
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OrderGeneratorXmlParserTest extends FreeSpec {

  "parse" in {
    val path = OrderGeneratorPath("/TEST")
    val timeZone = ZoneId.of("Europe/Berlin")
    val orderGenerator = OrderGeneratorXmlParser.parseXml(OrderGeneratorPath("/TEST"),
      <order job_chain="/JOBCHAIN" state="NODEID">
        <params>
          <param name="a" value="AAA"/>
        </params>
        <run_time>
          <period absolute_repeat="10"/>
        </run_time>
      </order>,
      timeZone)
    assert(orderGenerator == ScheduledOrderGenerator(
      path,
      NodeKey(JobChainPath("/JOBCHAIN"), NodeId("NODEID")),
      Map("a" → "AAA"),
      OldSchedule(timeZone, EveryDay(PeriodSeq(List(Period(absoluteRepeat = Some(10.s))))), startOnce = false)))
  }
}
