package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.workflow.notation.WorkflowParser
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.{JobPath, Position, WorkflowPath}
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.IfReturnCodeTest._
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

final class IfReturnCodeTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      directoryProvider.master.writeJson(TestWorkflow.path, TestWorkflow)
      for (a ← directoryProvider.agents) a.file(JobPath("/JOB"), SourceType.Xml).xml = <job><script language="shell">:</script></job>
      for (a ← directoryProvider.agents) a.file(JobPath("/JOB-RC"), SourceType.Xml).xml =
        <job>
          <script language="shell">{if (isWindows) "@exit %SCHEDULER_PARAM_RETURN_CODE%" else "exit $SCHEDULER_PARAM_RETURN_CODE"}</script>
        </job>

      directoryProvider.run { (master, _) ⇒
        val eventCollector = new TestEventCollector
        eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
        for (returnCode ← ExpectedEvents.keys) withClue(s"$returnCode: ") {
          val orderId = OrderId("🔺" + returnCode.number)
          master.addOrder(newOrder(orderId, returnCode)) await 99.s
          if (returnCode == ReturnCode(2))
            eventCollector.await[OrderStopped](_.key == orderId)
          else
            eventCollector.await[OrderFinished](_.key == orderId)
          checkEventSeq(orderId, eventCollector.all[OrderEvent], returnCode)
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]], returnCode: ReturnCode): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == ExpectedEvents(returnCode))
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object IfReturnCodeTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val script = """
      |job "JOB-RC" on "AGENT" successReturnCodes=(0, 1); // #0
      |if (returnCode 0) {      // #1
      |  job "JOB" on "AGENT";  // #1/0/0
      |} else {
      |  job "JOB" on "AGENT";  // #1/1/0
      |};
      |job "JOB" on "AGENT";    // #2
    """.stripMargin
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW"), script).force

  private val ExpectedEvents = Map(
    ReturnCode(0) → Vector(
      OrderAdded(TestWorkflow.path, Order.StartNow, Payload(Map("RETURN_CODE" → "0"))),
      OrderTransferredToAgent(TestAgentPath),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1, 0/*then*/, 0)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished),
    ReturnCode(1) → Vector(
      OrderAdded(TestWorkflow.path, Order.StartNow, Payload(Map("RETURN_CODE" → "1"))),
      OrderTransferredToAgent(TestAgentPath),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(1))),
      OrderMoved(Position(1, 1/*else*/, 0)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished),
    ReturnCode(2) →  Vector(
      OrderAdded(TestWorkflow.path, Order.StartNow, Payload(Map("RETURN_CODE" → "2"))),
      OrderTransferredToAgent(TestAgentPath),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(2))),
      OrderStopped(Outcome.Failed(ReturnCode(2)))))

  private def newOrder(orderId: OrderId, returnCode: ReturnCode) =
    Order(orderId, TestWorkflow.path, Order.StartNow, payload = Payload(Map("RETURN_CODE" → returnCode.number.toString)))
}
