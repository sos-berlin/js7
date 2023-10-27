package js7.tests

import izumi.reflect.Tag
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.instructions.{Fail, Retry, TryInstruction}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.tests.FailUncatchableTest.*
import js7.tests.jobs.{EmptyJob, FailingJob}
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced
import scala.reflect.ClassTag

final class FailUncatchableTest extends OurTestSuite:
  "fail" in:
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true);
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderOutcomeAdded(Outcome.failed.copy(uncatchable = true)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1))))

  "fail (uncatchable=true, returnCode=7)" in:
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, namedValues = { "returnCode": 7 });
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderOutcomeAdded(Outcome.Failed(namedValues = NamedValues.rc(7), uncatchable = true)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1))))

  "fail (uncatchable=true, returnCode=7, message='ERROR')" in:
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, namedValues = { "returnCode": 7 }, message='TEST-ERROR');
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderOutcomeAdded(Outcome.Failed(Some("TEST-ERROR"), NamedValues.rc(7), uncatchable = true)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1))))

  "fail in fork, fail first" in:
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork (joinIfFailed=true) {
     |    "🥕": {
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="sleep.cmd";
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          "🥕" -> OrderId("🔺|🥕"),
          "🍋" -> OrderId("🔺|🍋"))),
        OrderJoined(Outcome.Failed(Some("Order:🔺|🥕 Failed(uncatchable, TEST-ERROR)"))),
        OrderFailed(Position(0))))

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderOutcomeAdded(Outcome.Failed(Some("TEST-ERROR"), Map.empty, uncatchable = true)),
        // TODO OrderDetached, because agent does not has parent order and
        // cannot look at Fork.joinIfFailed. Okay because we join at Controller, anyway.
        OrderDetachable,
        OrderDetached,
        OrderFailedInFork(Position(0) / BranchId.fork("🥕") % 1)))
        //OrderDetachable,
        //OrderDetached))

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))

  "fail in fork, succeed first" in:
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork (joinIfFailed=true) {
     |    "🥕": {
     |      execute agent="AGENT", executable="sleep.cmd";
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          "🥕" -> OrderId("🔺|🥕"),
          "🍋" -> OrderId("🔺|🍋"))),
        OrderJoined(Outcome.Failed(Some("Order:🔺|🥕 Failed(uncatchable, TEST-ERROR)"))),
        OrderFailed(Position(0))))

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderOutcomeAdded(Outcome.Failed(Some("TEST-ERROR"), Map.empty, uncatchable = true)),
        // TODO OrderDetached early, because agent does not has parent order and
        // cannot look at Fork.joinIfFailed. Okay because we join at Controller, anyway.
        OrderDetachable,
        OrderDetached,
        OrderFailedInFork(Position(0) / BranchId.fork("🥕") % 1)))

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))

  "Uncatchable fail at Controller" in {
    val workflowId = TestWorkflowId
    checkEvents[OrderTerminated](
      Workflow(workflowId, Seq(
        TryInstruction(
          tryWorkflow = Workflow.of(
            Fail(uncatchable = true)),
          catchWorkflow = Workflow.empty))),
      Vector(
        OrderAdded(workflowId),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderStarted,
        OrderOutcomeAdded(Outcome.failed.copy(uncatchable = true)),
        OrderFailed(Position(0) / "try+0" % 0)))
  }

  "JS-2087 Uncatchable fail at Agent" in {
    val workflowId = TestWorkflowId
    checkEvents[OrderTerminated](
      Workflow(workflowId, Seq(
        EmptyJob.execute(agentPath = agentPath), // Move to Agent
        TryInstruction(
          tryWorkflow = Workflow.of(
            Fail(uncatchable = true)),
          catchWorkflow = Workflow.empty))),
      Vector(
        OrderAdded(workflowId),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(toLocalSubagentId(agentPath)),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(1) / "try+0" % 0),
        OrderOutcomeAdded(Outcome.failed.copy(uncatchable = true)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1) / "try+0" % 0)))
  }

  "JS-2087 Uncatchable fail leaves retry loop, Jira test case" in {
    val workflowId = TestWorkflowId
    checkEvents[OrderFailed](
      Workflow(workflowId, Seq(
        TryInstruction(
          tryWorkflow = Workflow.of(
            TryInstruction(
              tryWorkflow = Workflow.of(
                FailingJob.execute(agentPath)),
              catchWorkflow = Workflow.of(
                Fail(uncatchable = true)))),
          catchWorkflow = Workflow.of(
            Retry()),
          retryDelays = Option(Vector(100.s)),
          maxTries = Some(10)))),
      Vector(
        OrderAdded(workflowId),
        OrderMoved(Position(0) / "try+0" % 0 / "try+0" % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(toLocalSubagentId(agentPath)),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "try+0" % 0 / "catch+0" % 0),
        OrderOutcomeAdded(Outcome.failed.copy(uncatchable = true)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0) / "try+0" % 0 / "catch+0" % 0)))
  }

  private def checkEvents[E <: OrderEvent: ClassTag: Tag](workflowNotation: String, expectedEvents: Vector[OrderEvent]): Unit =
    assert(runUntil[E](workflowNotation).map(_.event) == expectedEvents)

  private def checkEvents[E <: OrderEvent: ClassTag: Tag](workflow: Workflow, expectedEvents: Vector[OrderEvent]): Unit =
    assert(runUntil[E](workflow).map(_.event) == expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag: Tag](workflowNotation: String): Vector[KeyedEvent[OrderEvent]] =
    runUntil[E](WorkflowParser.parse(TestWorkflowId, workflowNotation).orThrow)

  private def runUntil[E <: OrderEvent: ClassTag: Tag](workflow: Workflow): Vector[KeyedEvent[OrderEvent]] =
    val directoryProvider = new DirectoryProvider(
      agentPaths = Seq(agentPath), items = Seq(workflow), testName = Some("FailUncatchableTest"),
      controllerConfig = config"""
        js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
        js7.controller.agent-driver.command-batch-delay = 0ms
        js7.controller.agent-driver.event-buffer-delay = 0ms""",
      agentConfig = config"""
        js7.job.execution.signed-script-injection-allowed = yes"""
    )
    autoClosing(directoryProvider) { _ =>
      directoryProvider.agentEnvs.head.writeExecutable(RelativePathExecutable("test.cmd"), "exit 3")
      directoryProvider.agentEnvs.head
        .writeExecutable(RelativePathExecutable("sleep.cmd"), DirectoryProvider.script(100.ms))
      directoryProvider.run { (controller, _) =>
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        controller.eventWatch.await[E](_.key == orderId)
        controller.eventWatch
          .allKeyedEvents[OrderEvent]
          .view
          .filterNot(_.event.isInstanceOf[OrderStdWritten])
          .toVector
      }
    }


object FailUncatchableTest:
  private val orderId = OrderId("🔺")
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "INITIAL"
