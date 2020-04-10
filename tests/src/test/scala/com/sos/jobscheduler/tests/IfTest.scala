package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.{Else, Then}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.IfTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

final class IfTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, fileBased = TestWorkflow :: Nil, testName = Some("IfTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST$sh"), ":")
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/TEST-RC$sh"), jobScript)

      directoryProvider.run { (master, _) =>
        for (returnCode <- ExpectedEvents.keys) withClue(s"$returnCode: ") {
          val orderId = OrderId("🔺" + returnCode.number)
          master.addOrderBlocking(newOrder(orderId, returnCode))
          if (returnCode == ReturnCode(2))
            master.eventWatch.await[OrderFailed](_.key == orderId)
          else
            master.eventWatch.await[OrderFinished](_.key == orderId)
          checkEventSeq(orderId, master.eventWatch.all[OrderEvent], returnCode)
        }
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]], returnCode: ReturnCode): Unit = {
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
      OrderTransferredToMaster,
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
      OrderTransferredToMaster,
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
