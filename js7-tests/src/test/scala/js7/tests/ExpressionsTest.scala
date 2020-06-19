package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
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
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.tests.ExpressionsTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ExpressionsTest extends AnyFreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, fileBased = TestWorkflow :: Nil, testName = Some("ExpressionsTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST$sh"), ":")
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST-RC$sh"), jobScript)

      directoryProvider.run { (controller, _) =>
        for (order <- orders) withClue(s"Order ${order.id.string}: ") {
          controller.addOrderBlocking(order)
          val expectedLast = ExpectedEvents(order.id).last
          controller.eventWatch.await[OrderEvent](ke => ke.key == order.id && expectedLast.getClass == ke.event.getClass)
          checkEventSeq(order.id, controller.eventWatch.all[OrderEvent])
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.iterator.filter(_.value.key == orderId).map(_.value.event).to(Vector)
        assert(events == ExpectedEvents(orderId))
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object ExpressionsTest {
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
     |  if ($$ARG == "ARG-VALUE" && argument("ARG") == "ARG-VALUE") {
     |    LABEL: job MYJOB;
     |  }
     |  if ($$ARG2 == "ARG2-VALUE" && variable(key="JOB-KEY", label=LABEL) == "JOB-RESULT" && variable("JOB-KEY", job=MYJOB) == "JOB-RESULT") {
     |    if (returnCode == 1) {
     |      execute executable="/TEST$sh", agent="AGENT";
     |    }
     |  }
     |  if (returnCode == 0 && returnCode(label=LABEL) == 1) {
     |    execute executable="/TEST$sh", agent="AGENT";
     |  }
     |
     |  define job MYJOB {
     |    execute executable="/TEST-RC$sh", agent="AGENT", successReturnCodes=[0, 1];
     |  }
     |}""".stripMargin
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") ~ "INITIAL", workflowNotation).orThrow

  private val orders = Vector(
    FreshOrder(OrderId("❌"), TestWorkflow.id.path),
    FreshOrder(OrderId("⭕️"), TestWorkflow.id.path, arguments = Map("ARG" -> "ARG-VALUE")),
    FreshOrder(OrderId("🔺"), TestWorkflow.id.path, arguments = Map("ARG" -> "ARG-VALUE", "ARG2" -> "ARG2-VALUE", "RETURN_CODE" -> "1")))

  private val ExpectedEvents = Map(
    OrderId("❌") -> Vector(
      OrderAdded(TestWorkflow.id),
      OrderFailed(Outcome.Disrupted(Problem("No such named value: ARG")))),
    OrderId("⭕️") -> Vector(
      OrderAdded(TestWorkflow.id, None, Map("ARG" -> "ARG-VALUE")),
      OrderMoved(Position(0) / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(0), Map("JOB-KEY" -> "JOB-RESULT"))),
      OrderFailed(Outcome.Disrupted(Problem("No such named value: ARG2")))),
    OrderId("🔺") -> Vector(
      OrderAdded(TestWorkflow.id, None, Map("ARG" -> "ARG-VALUE", "ARG2" -> "ARG2-VALUE", "RETURN_CODE" -> "1")),
      OrderMoved(Position(0) / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(1), Map("JOB-KEY" -> "JOB-RESULT"))),
      OrderMoved(Position(1) / Then % 0 / Then % 0),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2) / Then % 0),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderTransferredToController,
      OrderFinished))
}
