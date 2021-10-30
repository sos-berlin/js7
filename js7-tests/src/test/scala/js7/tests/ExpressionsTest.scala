package js7.tests

import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.ExpressionsTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ExpressionsTest extends AnyFreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, items = Seq(TestWorkflow), testName = Some("ExpressionsTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(RelativePathExecutable(s"TEST$sh"), ":")
      for (a <- directoryProvider.agents) a.writeExecutable(RelativePathExecutable(s"TEST-RC$sh", v1Compatible = true), jobScript)

      directoryProvider.run { (controller, _) =>
        for (order <- orders) withClue(s"Order ${order.id.string}: ") {
          controller.addOrderBlocking(order)
          val expectedLast = ExpectedEvents(order.id).last
          controller.eventWatch.await[OrderEvent](ke => ke.key == order.id && expectedLast.getClass == ke.event.getClass)
          checkEventSeq(order.id, controller.eventWatch.allKeyedEvents[OrderEvent])
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, stampedSeq: Seq[KeyedEvent[OrderEvent]])
  : Unit = {
    val events = stampedSeq.view.filter(_.key == orderId).map(_.event).to(Vector)
    assert(events == ExpectedEvents(orderId))
  }
}

object ExpressionsTest {
  private val TestAgentPath = AgentPath("AGENT")

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
     |    if ($$returnCode == 1) {
     |      execute executable="TEST$sh", agent="AGENT";
     |    }
     |  }
     |  if ($$returnCode == 0 && variable("returnCode", label=LABEL) == 1) {
     |    execute executable="TEST$sh", agent="AGENT";
     |  }
     |
     |  define job MYJOB {
     |    execute executable="TEST-RC$sh", agent="AGENT", v1Compatible=true, successReturnCodes=[0, 1];
     |  }
     |}""".stripMargin
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("WORKFLOW") ~ "INITIAL", workflowNotation).orThrow

  private val orders = Vector(
    FreshOrder(OrderId("❌"), TestWorkflow.id.path),
    FreshOrder(OrderId("⭕️"), TestWorkflow.id.path, arguments = Map("ARG" -> StringValue("ARG-VALUE"))),
    FreshOrder(OrderId("🔺"), TestWorkflow.id.path, arguments = Map(
      "ARG" -> StringValue("ARG-VALUE"),
      "ARG2" -> StringValue("ARG2-VALUE"),
      "RETURN_CODE" -> StringValue("1"))))

  private val ExpectedEvents = Map(
    OrderId("❌") -> Vector(
      OrderAdded(TestWorkflow.id),
      OrderFailed(Position(0), Some(Outcome.Disrupted(Problem("No such named value: ARG"))))),
    OrderId("⭕️") -> Vector(
      OrderAdded(TestWorkflow.id, Map("ARG" -> StringValue("ARG-VALUE"))),
      OrderMoved(Position(0) / Then % 0),
      OrderAttachable(TestAgentPath),
      OrderAttached(TestAgentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(Map("JOB-KEY" -> StringValue("JOB-RESULT")) ++ NamedValues.rc(0))),
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(0) / Then % 0, Some(Outcome.Disrupted(Problem("No such named value: ARG2"))))),
    OrderId("🔺") -> Vector(
      OrderAdded(TestWorkflow.id, Map(
        "ARG" -> StringValue("ARG-VALUE"),
        "ARG2" -> StringValue("ARG2-VALUE"),
        "RETURN_CODE" -> StringValue("1"))),
      OrderMoved(Position(0) / Then % 0),
      OrderAttachable(TestAgentPath),
      OrderAttached(TestAgentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(Map("JOB-KEY" -> StringValue("JOB-RESULT")) ++ NamedValues.rc(1))),
      OrderMoved(Position(1) / Then % 0 / Then % 0),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(2) / Then % 0),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(3)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
}
