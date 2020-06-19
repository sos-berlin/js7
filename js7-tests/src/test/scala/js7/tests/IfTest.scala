package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.utils.AutoClosing.autoClosing
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.system.OperatingSystem.isWindows
import js7.data.agent.AgentRefPath
import js7.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent, OrderTransferredToController}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.Position
import js7.tests.IfTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class IfTest extends AnyFreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, fileBased = TestWorkflow :: Nil, testName = Some("IfTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST$sh"), ":")
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST-RC$sh"), jobScript)

      directoryProvider.run { (controller, _) =>
        for (returnCode <- ExpectedEvents.keys) withClue(s"$returnCode: ") {
          val orderId = OrderId("🔺" + returnCode.number)
          controller.addOrderBlocking(newOrder(orderId, returnCode))
          if (returnCode == ReturnCode(2))
            controller.eventWatch.await[OrderFailed](_.key == orderId)
          else
            controller.eventWatch.await[OrderFinished](_.key == orderId)
          checkEventSeq(orderId, controller.eventWatch.all[OrderEvent], returnCode)
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]], returnCode: ReturnCode): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.iterator.filter(_.value.key == orderId).map(_.value.event).to(Vector)
        assert(events == ExpectedEvents(returnCode))
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object IfTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")

  private val jobScript =
    if (isWindows)
      """@echo off
        |echo JOB-KEY=JOB-RESULT >>%SCHEDULER_RETURN_VALUES%
        |exit %SCHEDULER_PARAM_RETURN_CODE%
        |""".stripMargin
    else
      """echo JOB-KEY=JOB-RESULT >>"$SCHEDULER_RETURN_VALUES"
        |exit $SCHEDULER_PARAM_RETURN_CODE
        |""".stripMargin

  private val workflowNotation = s"""
     |define workflow {
     |  if ($$ARG == "ARG-VALUE") {
     |    if (argument("ARG") == "ARG-VALUE") {
     |      LABEL: job MYJOB;  // 0/then:0/then:0
     |    }
     |  }
     |  if ($$ARG != "X" && variable(key="JOB-KEY", label=LABEL) == "JOB-RESULT" && variable("JOB-KEY", job=MYJOB) == "JOB-RESULT") {
     |    if (returnCode(label=LABEL) == 0) {
     |      execute executable="/TEST$sh", agent="AGENT";
     |    } else {
     |      execute executable="/TEST$sh", agent="AGENT";
     |    }
     |  }
     |  execute executable="/TEST$sh", agent="AGENT";
     |
     |  define job MYJOB {
     |    execute executable="/TEST-RC$sh", agent="AGENT", successReturnCodes=[0, 1];
     |  }
     |}""".stripMargin
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") ~ "INITIAL", workflowNotation).orThrow

  private val ExpectedEvents = Map(
    ReturnCode(0) -> Vector(
      OrderAdded(TestWorkflow.id, None, Map("ARG" -> "ARG-VALUE", "RETURN_CODE" -> "0")),
      OrderMoved(Position(0) / Then % 0 / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(0), Map("JOB-KEY" -> "JOB-RESULT"))),
      OrderMoved(Position(1) / Then % 0 / Then % 0),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToController,
      OrderFinished),
    ReturnCode(1) -> Vector(
      OrderAdded(TestWorkflow.id, None, Map("ARG" -> "ARG-VALUE", "RETURN_CODE" -> "1")),
      OrderMoved(Position(0) / Then % 0 / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(1), Map("JOB-KEY" -> "JOB-RESULT"))),
      OrderMoved(Position(1) / Then % 0 / Else % 0),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToController,
      OrderFinished),
    ReturnCode(2) ->  Vector(
      OrderAdded(TestWorkflow.id, None, Map("ARG" -> "ARG-VALUE", "RETURN_CODE" -> "2")),
      OrderMoved(Position(0) / Then % 0 / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(2), Map("JOB-KEY" -> "JOB-RESULT"))),
      OrderFailed(Outcome.Failed(ReturnCode(2), Map("JOB-KEY" -> "JOB-RESULT")))))    // TODO Key-values in OrderFailed ?

  private def newOrder(orderId: OrderId, returnCode: ReturnCode) =
    FreshOrder(orderId, TestWorkflow.id.path, arguments = Map(
      "ARG" -> "ARG-VALUE",
      "RETURN_CODE" -> returnCode.number.toString))
}
