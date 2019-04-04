package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.Then
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.ExpressionsTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

final class ExpressionsTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, fileBased = TestWorkflow :: Nil)) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST$sh"), ":")
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST-RC$sh"), jobScript)

      directoryProvider.run { (master, _) =>
        for (order <- orders) withClue(s"Order ${order.id.string}: ") {
          master.addOrderBlocking(order)
          val expectedLast = ExpectedEvents(order.id).last
          master.eventWatch.await[OrderEvent](ke => ke.key == order.id && expectedLast.getClass == ke.event.getClass)
          checkEventSeq(order.id, master.eventWatch.all[OrderEvent])
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
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
      """echo JOB-KEY=JOB-RESULT >>%SCHEDULER_RETURN_VALUES%
        |@exit %SCHEDULER_PARAM_RETURN_CODE%
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
      OrderStopped(Outcome.Disrupted(Problem("No such named value: ARG")))),
    OrderId("⭕️") -> Vector(
      OrderAdded(TestWorkflow.id, None, Map("ARG" -> "ARG-VALUE")),
      OrderMoved(Position(0) / Then % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(0), Map("JOB-KEY" -> "JOB-RESULT"))),
      OrderStopped(Outcome.Disrupted(Problem("No such named value: ARG2")))),
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
      OrderTransferredToMaster,
      OrderFinished))
}
