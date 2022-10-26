package js7.tests

import izumi.reflect.Tag
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ReturnCode
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.KeyedEvent
import js7.data.job.{RelativePathExecutable, ReturnCodeMeaning}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, Finish, Fork, If}
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.FinishTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest, DirectoryProvider}
import monix.execution.Scheduler.Implicits.traced
import scala.reflect.ClassTag

final class FinishTest
extends OurTestSuite with ControllerAgentForScalaTest with BlockingItemUpdater
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  protected def agentPaths = Seq(agentPath)
  protected def items = Nil

  override def beforeAll() = {
    super.beforeAll()
    directoryProvider.agents.head.writeExecutable(RelativePathExecutable("test.cmd"), "exit 3")
    directoryProvider.agents.head.writeExecutable(RelativePathExecutable("sleep.cmd"), DirectoryProvider.script(100.ms))
  }

  "finish" in {
    val orderId = OrderId("🟠")
    checkEvents[OrderFinished](
      Workflow.of(
        executeSuccess,
        Finish(),
        Fail()),
      orderId,
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished()))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0), Outcome.Succeeded(Map(
        "returnCode" -> NumberValue(3))))))
  }

  "finish with if" in {
    val orderId = OrderId("🟢")
    checkEvents[OrderFinished](
      Workflow.of(
        executeSuccess,
        If(expr("true"),
          Workflow.of(
            executeSuccess,
            Finish(Some(Outcome.Failed(Some("FAIL WITH FINISH")))))),
        Fail()),
      orderId,
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1) / "then" % 0),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1) / "then" % 1),
        OrderDetachable,
        OrderDetached,
        OrderFinished(Some(Outcome.Failed(Some("FAIL WITH FINISH"))))))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0), Outcome.Succeeded(Map("returnCode" -> NumberValue(3)))),
      HistoricOutcome(Position(1) / "then" % 0, Outcome.Succeeded(Map("returnCode" -> NumberValue(3)))),
      HistoricOutcome(Position(1) / "then" % 1, Outcome.Failed(Some("FAIL WITH FINISH")))))
  }

  "finish in fork, finish first" in {
    val orderId = OrderId("🔵")
    val events = runUntil[OrderTerminated](
      Workflow.of(
        Fork(
          Vector(
            "🥕" -> Workflow.of(
              executeSuccess,
              If(expr("true"),
                Workflow.of(
                  Finish())),
              executeSuccess),
            "🍋" -> Workflow.of(
              executeSleep,
              Finish(Some(Outcome.Succeeded(Map(
                "result" -> StringValue("FINISH"))))))))),
      orderId)

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
        OrderDetachable,
        OrderDetached,
        OrderMoved(Position(0) / "fork+🥕" % 3)))    // Moved to end

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached,
        OrderOutcomeAdded(Outcome.Succeeded(Map(
          "result" -> StringValue("FINISH")))),
        OrderMoved(Position(0) / "fork+🍋" % 2)))

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), orderId / "🥕"),
          OrderForked.Child(Fork.Branch.Id("🍋"), orderId / "🍋"))),
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished()))

    assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
      HistoricOutcome(Position(0), Outcome.succeeded)))
  }

  "finish in fork, succeed first" in {
    val workflow = Workflow.of(
      Fork(
        Vector(
          "🥕" -> Workflow.of(
            executeSleep,
            If(expr("true"),
              Workflow.of(
                Finish(Some(Outcome.Failed(Some("FAIL WITH FINISH")))))),
            executeFailure),
          "🍋" -> Workflow.of(
            executeSuccess))))

    val orderId = OrderId("🟣")

    withTemporaryItem(workflow.withId(workflowId.path)) { workflow =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      eventWatch.await[OrderFailed](_.key == orderId / "🥕")

      assert(eventWatch.eventsByKey[OrderEvent](orderId / "🥕")
        .filterNot(_.isInstanceOf[OrderStdWritten]) ==
        Vector(
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0), // Position of Finish
          OrderDetachable,
          OrderDetached,
          OrderOutcomeAdded(Outcome.Failed(Some("FAIL WITH FINISH"))),
          OrderFailed(Position(0) / "fork+🥕" % 1 / Then % 0)))

      assert(controllerState.idToOrder(orderId / "🥕").historicOutcomes == Seq(
        HistoricOutcome(Position(0) / "fork+🥕" % 0,
          Outcome.Succeeded(Map("returnCode" -> NumberValue(0)))),
        HistoricOutcome(Position(0) / "fork+🥕" % 1 / "then" % 0,
          Outcome.Failed(Some("FAIL WITH FINISH")))))

      controllerApi.executeCommand(CancelOrders(Seq(orderId / "🥕"))).await(99.s).orThrow
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
          OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
          OrderMoved(Position(0) / "fork+🍋" % 1),
          OrderDetachable,
          OrderDetached))

      assert(events.filter(_.key == orderId).map(_.event) ==
        Vector(
          OrderStarted,
          OrderForked(Vector(
            OrderForked.Child(Fork.Branch.Id("🥕"), orderId / "🥕"),
            OrderForked.Child(Fork.Branch.Id("🍋"), orderId / "🍋"))),
          OrderJoined(Outcome.Failed(Some("Order:🟣|🥕 has been cancelled"))),
          OrderFailed(Position(0))))

      assert(controllerState.idToOrder(orderId).historicOutcomes == Seq(
        HistoricOutcome(Position(0), Outcome.Failed(Some("Order:🟣|🥕 has been cancelled")))))
    }
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
    withTemporaryItem(workflow.withId(workflowId.path)) { workflow =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      eventWatch.await[E](_.key == orderId)

      eventWatch
        .allKeyedEvents[OrderEvent]
        .view
        .filterNot(_.event.isInstanceOf[OrderAdded])
        .filterNot(_.event.isInstanceOf[OrderStdWritten])
        .toVector
    }
}

object FinishTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val workflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val executeFailure = Execute.Anonymous(
    WorkflowJob(
      agentPath,
      RelativePathExecutable(
        "test.cmd")))

  private val executeSuccess = Execute.Anonymous(
    WorkflowJob(
      agentPath,
      RelativePathExecutable(
        "test.cmd",
        returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(3))))))

  private val executeSleep = Execute.Anonymous(
    WorkflowJob(
      agentPath,
      RelativePathExecutable(
        "sleep.cmd")))
}
