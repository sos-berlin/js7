package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.IfTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

final class IfTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, fileBased = TestWorkflow :: Nil)) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST$sh"), ":")
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST-RC$sh"),
        if (isWindows) "@exit %SCHEDULER_PARAM_RETURN_CODE%"
        else "exit $SCHEDULER_PARAM_RETURN_CODE")

      directoryProvider.run { (master, _) =>
        for (returnCode <- ExpectedEvents.keys) withClue(s"$returnCode: ") {
          val orderId = OrderId("🔺" + returnCode.number)
          master.addOrderBlocking(newOrder(orderId, returnCode))
          if (returnCode == ReturnCode(2))
            master.eventWatch.await[OrderStopped](_.key == orderId)
          else
            master.eventWatch.await[OrderFinished](_.key == orderId)
          checkEventSeq(orderId, master.eventWatch.all[OrderEvent], returnCode)
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]], returnCode: ReturnCode): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == ExpectedEvents(returnCode))
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object IfTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val script = s"""
     |define workflow {
     |  if (true) {
     |    if (true) {
     |      execute executable="/TEST-RC$sh", agent="AGENT", successReturnCodes=[0, 1]; // :0/then:0
     |    }
     |  }
     |  if (true) {
     |    if (returnCode == 0) {   // :2
     |      execute executable="/TEST$sh", agent="AGENT";  // :2/then:0
     |    } else {
     |      execute executable="/TEST$sh", agent="AGENT";  // :2/else:0
     |    }
     |  }
     |  execute executable="/TEST$sh", agent="AGENT";    // :2
     |}""".stripMargin
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") % "INITIAL", script).orThrow

  private val ExpectedEvents = Map(
    ReturnCode(0) -> Vector(
      OrderAdded(TestWorkflow.id, None, Payload(Map("RETURN_CODE" -> "0"))),
      OrderMoved(Position(0) / Then % 0 / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1) / Then % 0 / Then % 0),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished),
    ReturnCode(1) -> Vector(
      OrderAdded(TestWorkflow.id, None, Payload(Map("RETURN_CODE" -> "1"))),
      OrderMoved(Position(0) / Then % 0 / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(1))),
      OrderMoved(Position(1) / Then % 0 / Else % 0),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished),
    ReturnCode(2) ->  Vector(
      OrderAdded(TestWorkflow.id, None, Payload(Map("RETURN_CODE" -> "2"))),
      OrderMoved(Position(0) / Then % 0 / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(2))),
      OrderStopped(Outcome.Failed(ReturnCode(2)))))

  private def newOrder(orderId: OrderId, returnCode: ReturnCode) =
    FreshOrder(orderId, TestWorkflow.id.path, payload = Payload(Map("RETURN_CODE" -> returnCode.number.toString)))
}
