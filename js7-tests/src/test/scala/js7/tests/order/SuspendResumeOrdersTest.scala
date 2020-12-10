package js7.tests.order

import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.process.ProcessSignal.SIGTERM
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.{Batch, CancelOrders, Response, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.controller.data.events.AgentRefStateEvent.AgentReady
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.AgentId
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.item.VersionId
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderResumeMarked, OrderResumed, OrderRetrying, OrderStarted, OrderStdWritten, OrderStdoutWritten, OrderSuspendMarked, OrderSuspended}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderEvent, OrderId, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem}
import js7.data.value.{BooleanValue, NamedValues}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, Fork, Retry, TryInstruction}
import js7.data.workflow.position.BranchId.{Try_, catch_, try_}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.SuspendResumeOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class SuspendResumeOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val inventoryItems = singleJobWorkflow :: twoJobsWorkflow :: forkWorkflow :: tryWorkflow :: Nil
  override def controllerConfig = config"js7.journal.remove-obsolete-files = false" withFallback super.controllerConfig

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(executablePath, script(300.ms))
      a.writeExecutable(quickExecutablePath, script(0.s))
    }
    super.beforeAll()
  }

  "Suspend and resume a fresh order" in {
    controller.eventWatch.await[AgentReady]()
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 2.s))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderAttached](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Seq(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderSuspendMarked(),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))
    val lastEventId = controller.eventWatch.lastAddedEventId

    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow

    // ResumeOrders command expected a suspended or suspending order
    assert(controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s) == Left(CannotResumeOrderProblem))

    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(None),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderStdoutWritten("TEST ☘\n"),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "An order reaching end of workflow is suspendible" in {
    val order = FreshOrder(OrderId("⭕️"), singleJobWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = controller.eventWatch.lastAddedEventId
    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(None),
      OrderFinished))
  }

  "Suspend with kill" in {
    val order = FreshOrder(OrderId("🟥"), singleJobWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    val suspendOrders = SuspendOrders(Set(order.id), SuspendMode(Some(CancelMode.Kill())))
    controller.executeCommandAsSystemUser(suspendOrders).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(SuspendMode(Some(CancelMode.Kill()))),
      OrderProcessed(Outcome.Killed(if (isWindows) Outcome.succeededRC0 else Outcome.Failed(NamedValues.rc(SIGTERM)))),
      OrderProcessingKilled,
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = controller.eventWatch.lastAddedEventId
    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(None),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderProcessingStarted,
      OrderStdoutWritten("TEST ☘\n"),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Suspend and resume orders between two jobs" in {
    val orders = Seq(FreshOrder(OrderId("🔴"), twoJobsWorkflow.path), FreshOrder(OrderId("♦️"), twoJobsWorkflow.path))
    for (order <- orders) controller.addOrderBlocking(order)
    for (order <- orders) controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(orders.map(_.id))).await(99.s).orThrow
    for (order <- orders) controller.eventWatch.await[OrderSuspended](_.key == order.id)
    for (order <- orders) {
      assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderAdded(twoJobsWorkflow.id, order.scheduledFor),
        OrderAttachable(agentId),
        OrderAttached(agentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderSuspendMarked(),
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderSuspended))
    }

    val lastEventId = controller.eventWatch.lastAddedEventId
    controller.executeCommandAsSystemUser(ResumeOrders(orders.map(_.id))).await(99.s).orThrow
    for (order <- orders) controller.eventWatch.await[OrderFinished](_.key == order.id)
    for (order <- orders) {
      assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId)
        .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
          OrderResumed(None),
          OrderAttachable(agentId),
          OrderAttached(agentId),
          OrderProcessingStarted,
          OrderProcessed(Outcome.succeededRC0),
          OrderMoved(Position(2)),
          OrderDetachable,
          OrderDetached,
          OrderFinished))
      }
  }

  "An order being cancelled is not suspendible nor resumable" in {
    val order = FreshOrder(OrderId("🔷"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOrStarted())).await(99.s).orThrow
    assert(controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
  }

  "Suspend a forked order - child orders are not suspended" in {
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == (order.id | "🥕"))

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Seq(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Seq(OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("FORK|🥕")))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentId),
        OrderId("FORK|🥕") <-: OrderAttached(agentId),
        OrderId("FORK|🥕") <-: OrderProcessingStarted,
        OrderId("FORK") <-: OrderSuspendMarked(),
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderProcessingStarted,
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 2),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderSuspended))
  }

  "Suspend unknown order" in {
    assert(controller.executeCommandAsSystemUser(SuspendOrders(Set(OrderId("UNKNOWN")))).await(99.s) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Suspend multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield
      FreshOrder(OrderId(i.toString), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.s))
    for (o <- orders) controller.addOrderBlocking(o)
    for (o <- orders) controller.eventWatch.await[OrderAttached](_.key == o.id)
    val response = controller.executeCommandAsSystemUser(Batch(for (o <- orders) yield SuspendOrders(Set(o.id)))).await(99.s).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) controller.eventWatch.await[OrderSuspended](_.key == o.id)
  }

  "Resume a still suspending order" in {
    val order = FreshOrder(OrderId("🔹"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderResumeMarked(None),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),

      // AgentOrderKeeper does not properly handle simulataneous ExecuteMarkOrder commands
      // and so order is detached for suspending (which has been withdrawn by ResumeOrders).
      OrderDetachable,
      OrderDetached,
      OrderAttachable(agentId),
      OrderAttached(agentId),

      OrderProcessingStarted,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Resume with position a still suspending order is inhibited" in {
    val order = FreshOrder(OrderId("🔵"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    assert(controller.executeCommandAsSystemUser(ResumeOrder(order.id, Some(Position(0)))).await(99.s) ==
      Left(CannotResumeOrderProblem))

    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Resume with invalid position is rejected" in {
    val order = FreshOrder(OrderId("INVALID-POSITION"), tryWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(
      controller.executeCommandAsSystemUser(ResumeOrder(order.id, Some(Position(99)))).await(99.s) ==
        Left(Problem("ResumeOrder: Unreachable order position")))
    assert(
      controller.executeCommandAsSystemUser(ResumeOrder(order.id,
        historicOutcomes = Some(Seq(HistoricOutcome(Position(99), Outcome.succeeded))))
      ).await(99.s) == Left(Problem("Unknown position 99 in workflow 'Workflow:/TRY~INITIAL'")))
  }

  "Resume with changed position and changed historic outcomes" in {
    val order = FreshOrder(OrderId("🔶"), tryWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(tryWorkflow.id, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = controller.eventWatch.lastAddedEventId
    val newPosition = Position(2) / Try_ % 0
    val newHistoricOutcomes = Seq(
      HistoricOutcome(Position(0), Outcome.Succeeded(Map("NEW" -> BooleanValue(true)))),
      HistoricOutcome(Position(1), Outcome.failed))

    controller.executeCommandAsSystemUser(ResumeOrder(order.id, Some(newPosition), Some(newHistoricOutcomes)))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId)
      .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderResumed(Some(newPosition), Some(newHistoricOutcomes)),
        OrderCatched(Position(2) / catch_(0) % 0, Some(Outcome.failed)),
        OrderRetrying(Position(2) / try_(1) % 0, None),
        OrderFailed(Position(2) / try_(1) % 0, Some(Outcome.failed))))

    assert(controller.orderApi.order(order.id).await(99.s) == Right(Some(Order(
      order.id, order.workflowPath ~ "INITIAL" /: (Position(2) / try_(1) % 0),
      Order.Failed,
      historicOutcomes = newHistoricOutcomes :+
        HistoricOutcome(Position(2) / Try_ % 0, Outcome.failed) :+
        HistoricOutcome(Position(2) / try_(1) % 0, Outcome.failed)))))
  }
}

object SuspendResumeOrdersTest
{
  private val executablePath = RelativeExecutablePath("executable.cmd")
  private val quickExecutablePath = RelativeExecutablePath("quick.cmd")
  private val agentId = AgentId("AGENT")
  private val versionId = VersionId("INITIAL")

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ versionId,
    Execute(WorkflowJob(agentId, executablePath)))

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("/TWO") ~ versionId,
    Execute(WorkflowJob(agentId, executablePath, taskLimit = 10)),
    Execute(WorkflowJob(agentId, executablePath, taskLimit = 10)))

  private val forkWorkflow = Workflow.of(
    WorkflowPath("/FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
        Execute(WorkflowJob(agentId, executablePath)),
        Execute(WorkflowJob(agentId, executablePath)))),
    Execute(WorkflowJob(agentId, executablePath)))

  private val tryWorkflow = Workflow.of(
    WorkflowPath("/TRY") ~ versionId,
    Execute(WorkflowJob(agentId, executablePath)),
    Execute(WorkflowJob(agentId, executablePath)),
    TryInstruction(
      Workflow.of(
        Fail()),
      Workflow.of(
        Retry()),
      maxTries = Some(2)))
}
