package js7.tests

import cats.effect.unsafe.IORuntime
import izumi.reflect.Tag
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode.FreshOrStarted
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancelled, OrderCaught, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, Finish, Fork, If, TryInstruction}
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.FinishTest.*
import js7.tests.jobs.{EmptyJob, FailingJob, SleepJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.reflect.ClassTag

final class FinishTest
extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected def agentPaths = Seq(agentPath)
  protected def items = Nil

  "finish" in:
    val orderId = OrderId("♣️")
    checkEvents[OrderFinished](
      Workflow.of(
        EmptyJob.execute(agentPath),
        Finish(),
        Fail()),
      orderId,
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished()))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0), OrderOutcome.succeeded)))

  "Successful Finish in catch (JS-2073)" in:
    val orderId = OrderId("SUCCESSFUL-FINISH")
    checkEvents[OrderFinished](
      Workflow.of(
        TryInstruction(
          Workflow.of(
            FailingJob.execute(agentPath)),
          Workflow.of(
            Finish(Some(OrderOutcome.Succeeded(Map("result" -> StringValue("SUCCESS"))))))),
        Fail()),
      orderId,
      Vector(
        OrderMoved(Position(0) / "try+0" % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "catch+0" % 0),
        OrderDetachable,
        OrderDetached,
        OrderFinished(Some(OrderOutcome.Succeeded(Map("result" -> StringValue("SUCCESS")))))))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0) / "try+0" % 0, FailingJob.outcome),
      HistoricOutcome(Position(0) / "catch+0" % 0, OrderOutcome.Caught),
      HistoricOutcome(Position(0) / "catch+0" % 0, OrderOutcome.Succeeded(Map(
        "result" -> StringValue("SUCCESS"))))))

  "finish with if" in:
    val orderId = OrderId("♠️")
    checkEvents[OrderFinished](
      Workflow.of(
        EmptyJob.execute(agentPath),
        If(expr("true")):
          Workflow.of(
            EmptyJob.execute(agentPath),
            Finish(Some(OrderOutcome.Failed(Some("FAIL WITH FINISH"))))),
        Fail()),
      orderId,
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1) / "then" % 0),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1) / "then" % 1),
        OrderDetachable,
        OrderDetached,
        OrderFinished(Some(OrderOutcome.Failed(Some("FAIL WITH FINISH"))))))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1) / "then" % 0, OrderOutcome.succeeded),
      HistoricOutcome(Position(1) / "then" % 1, OrderOutcome.Failed(Some("FAIL WITH FINISH")))))

  "finish in fork, finish first" in:
    val orderId = OrderId("♥️")
    val events = runUntil[OrderTerminated](
      Workflow.of:
        Fork(
          Vector(
            "🥕" -> Workflow.of(
              EmptyJob.execute(agentPath),
              If(expr("true")):
                Finish(),
              EmptyJob.execute(agentPath)),
            "🍋" -> Workflow.of(
              SleepJob.sleep(agentPath, 100.ms),
              Finish(Some(OrderOutcome.Succeeded(Map(
                "result" -> StringValue("FINISH")))))))),
      orderId)

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
        OrderDetachable,
        OrderDetached,
        OrderMoved(Position(0) / "fork+🥕" % 3)))    // Moved to end

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached,
        OrderOutcomeAdded(OrderOutcome.Succeeded(Map(
          "result" -> StringValue("FINISH")))),
        OrderMoved(Position(0) / "fork+🍋" % 2)))

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderStarted,
        OrderForked(Vector(
          "🥕" -> orderId / "🥕",
          "🍋" -> orderId / "🍋")),
        OrderJoined(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished()))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0), OrderOutcome.succeeded)))

  "JS-2067 is not a bug" - {
    "Failing Finish in fork, with joinIfFailed" in:
      val orderId = OrderId("♦️")
      val events = runUntil[OrderTerminated](
        Workflow.of(
          Fork(
            Vector(
              "🥕" -> Workflow.of(
                EmptyJob.execute(agentPath),
                If(expr("true")):
                  Finish(Some(OrderOutcome.Failed(Some("FINISH WITH FAILURE")))),
                EmptyJob.execute(agentPath)),
              "🍋" -> Workflow.of(
                Finish(Some(OrderOutcome.Succeeded(Map(
                  "result" -> StringValue("FINISH"))))))),
            joinIfFailed = true)),
        orderId)

      assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
        Vector(
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
          OrderDetachable,
          OrderDetached,
          OrderOutcomeAdded(OrderOutcome.Failed(Some("FINISH WITH FAILURE"))),
          OrderFailedInFork(Position(0) / "fork+🥕" %  1 / "then" % 0)))

      assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
        Vector(
          OrderOutcomeAdded(OrderOutcome.Succeeded(Map(
            "result" -> StringValue("FINISH")))),
          OrderMoved(Position(0) / "fork+🍋" % 1)))

      assert(events.filter(_.key == orderId).map(_.event) ==
        Vector(
          OrderStarted,
          OrderForked(Vector(
            "🥕" -> orderId / "🥕",
            "🍋" -> orderId / "🍋")),
          OrderJoined(OrderOutcome.Failed(Some("Order:♦️|🥕 Failed(FINISH WITH FAILURE)"))),
          OrderFailed(Position(0))))

      assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
        HistoricOutcome(Position(0), OrderOutcome.Failed(Some("Order:♦️|🥕 Failed(FINISH WITH FAILURE)")))))

    "Failing Finish in fork, without joinIfFailed" in:
      val workflow = Workflow.of(
        Fork(
          Vector(
            "🥕" -> Workflow.of(
              EmptyJob.execute(agentPath),
              If(expr("true")):
                Finish(Some(OrderOutcome.Failed(Some("FINISH WITH FAILURE")))),
              EmptyJob.execute(agentPath)),
            "🍋" -> Workflow.of(
              Finish(Some(OrderOutcome.Succeeded(Map(
                "result" -> StringValue("FINISH")))))))))

      val orderId = OrderId("🔔")
      withItem(workflow.withId(workflowId.path)) { workflow =>
        controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
        eventWatch.await[OrderFailed](_.key == orderId / "🥕")
        sleep(100.ms)
        assert(controllerState.idToOrder(orderId).isState[Order.Forked])

        controller.api.executeCommand(CancelOrders(Seq(orderId, orderId / "🥕", orderId / "🍋")))
          .await(99.s).orThrow
        controller.eventWatch.await[OrderCancelled](_.key == orderId)

        val events = eventWatch
          .allKeyedEvents[OrderEvent]
          .view
          .filterNot(_.event.isInstanceOf[OrderAdded])
          .filterNot(_.event.isInstanceOf[OrderStdWritten])
          .toVector

        assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
          Vector(
            OrderAttachable(agentPath),
            OrderAttached(agentPath),
            OrderProcessingStarted(subagentId),
            OrderProcessed(OrderOutcome.succeeded),
            OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0), // Position of Finish
            OrderDetachable,
            OrderDetached,
            OrderOutcomeAdded(OrderOutcome.Failed(Some("FINISH WITH FAILURE"))),
            OrderFailed(Position(0) / "fork+🥕" % 1 / "then" % 0),
            OrderCancelled))

        assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
          Vector(
            OrderOutcomeAdded(OrderOutcome.Succeeded(Map(
              "result" -> StringValue("FINISH")))),
            OrderMoved(Position(0) / "fork+🍋" % 1),
            OrderCancellationMarked(FreshOrStarted())))

        assert(events.filter(_.key == orderId).map(_.event) ==
          Vector(
            OrderStarted,
            OrderForked(Vector(
              "🥕" -> orderId / "🥕",
              "🍋" -> orderId / "🍋")),
            OrderCancellationMarked(FreshOrStarted()),
            OrderJoined(OrderOutcome.Failed(Some("Order:🔔|🥕 has been cancelled"))),
            OrderFailed(Position(0)),
            OrderCancelled))

        assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
          HistoricOutcome(Position(0), OrderOutcome.Failed(Some("Order:🔔|🥕 has been cancelled")))))
      }
  }

  "finish in fork, succeed first" in:
    val workflow = Workflow.of(
      Fork(
        Vector(
          "🥕" -> Workflow.of(
            SleepJob.sleep(agentPath, 100.ms),
            If(expr("true")):
              Finish(Some(OrderOutcome.Failed(Some("FAIL WITH FINISH")))),
            FailingJob.execute(agentPath)),
          "🍋" -> Workflow.of(
            EmptyJob.execute(agentPath)))))

    val orderId = OrderId("🟪")

    withItem(workflow.withId(workflowId.path)) { workflow =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      eventWatch.await[OrderFailed](_.key == orderId / "🥕")

      assert(eventWatch.eventsByKey[OrderEvent](orderId / "🥕")
        .filterNot(_.isInstanceOf[OrderStdWritten]) ==
        Vector(
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0), // Position of Finish
          OrderDetachable,
          OrderDetached,
          OrderOutcomeAdded(OrderOutcome.Failed(Some("FAIL WITH FINISH"))),
          OrderFailed(Position(0) / "fork+🥕" % 1 / Then % 0)))

      assert(controllerState.idToOrder(orderId / "🥕").historicOutcomes == Seq(
        HistoricOutcome(Position(0) / "fork+🥕" % 0, OrderOutcome.succeeded),
        HistoricOutcome(Position(0) / "fork+🥕" % 1 / "then" % 0,
          OrderOutcome.Failed(Some("FAIL WITH FINISH")))))

      controller.api.executeCommand(CancelOrders(Seq(orderId / "🥕"))).await(99.s).orThrow
      eventWatch.await[OrderFailed](_.key == orderId)
      val events = eventWatch
        .allKeyedEvents[OrderEvent]
        .view
        .filterNot(_.event.isInstanceOf[OrderAdded])
        .filterNot(_.event.isInstanceOf[OrderStdWritten])
        .toVector

      assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
        Vector(
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(0) / "fork+🍋" % 1),
          OrderDetachable,
          OrderDetached))

      assert(events.filter(_.key == orderId).map(_.event) ==
        Vector(
          OrderStarted,
          OrderForked(Vector(
            "🥕" -> orderId / "🥕",
            "🍋" -> orderId / "🍋")),
          OrderJoined(OrderOutcome.Failed(Some("Order:🟪|🥕 has been cancelled"))),
          OrderFailed(Position(0))))

      assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
        HistoricOutcome(Position(0), OrderOutcome.Failed(Some("Order:🟪|🥕 has been cancelled")))))
    }

  private def checkEvents[E <: OrderEvent: ClassTag: Tag](
    workflow: Workflow,
    orderId: OrderId,
    expectedEvents: Vector[OrderEvent])
  : Unit =
    assert(runUntil[E](workflow, orderId)
      .filter(_.key == orderId)
      .map(_.event) == expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag: Tag](workflow: Workflow, orderId: OrderId)
  : Vector[KeyedEvent[OrderEvent]] =
    withItem(workflow.withId(workflowId.path)) { workflow =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      eventWatch.await[E](_.key == orderId)

      eventWatch
        .allKeyedEvents[OrderEvent]
        .view
        .filterNot(_.event.isInstanceOf[OrderAdded])
        .filterNot(_.event.isInstanceOf[OrderStdWritten])
        .toVector
    }


object FinishTest:

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val workflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
