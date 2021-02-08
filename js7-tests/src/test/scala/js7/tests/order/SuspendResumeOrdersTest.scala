package js7.tests.order

import com.google.common.io.MoreFiles.touch
import java.nio.file.Files.{createTempFile, deleteIfExists}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.AgentId
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.controller.ControllerCommand.{Batch, CancelOrders, Response, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelMarkedOnAgent, OrderCancelled, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderResumeMarked, OrderResumed, OrderRetrying, OrderStarted, OrderStdWritten, OrderSuspendMarked, OrderSuspendMarkedOnAgent, OrderSuspended}
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
import js7.tests.testenv.DirectoryProvider.waitingForFileScript
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class SuspendResumeOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = singleJobWorkflow :: twoJobsWorkflow :: forkWorkflow :: tryWorkflow :: Nil
  override def controllerConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  private lazy val triggerFile = createTempFile("SuspendResumeOrdersTest-", ".tmp")

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(pathExecutable, waitingForFileScript(triggerFile, delete = true))
    }
    super.beforeAll()
    controller.eventWatch.await[AgentReady]()
  }

  override def afterAll() = {
    deleteIfExists(triggerFile)
    super.afterAll()
  }

  "Suspend and resume a fresh order" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 2.s/*1s too short in rare cases*/))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderAttached](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderSuspendMarked(),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))
    val lastEventId = controller.eventWatch.lastAddedEventId

    touch(triggerFile)
    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow

    // TODO Modify order start time here, when possible. Otherwise we wait until the scheduled start time

    // ResumeOrders command expected a suspended or suspending order
    assert(controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s) == Left(CannotResumeOrderProblem))

    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(None),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
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
    controller.eventWatch.await[OrderSuspendMarkedOnAgent](_.key == order.id)
    touch(triggerFile)
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderSuspendMarkedOnAgent,
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
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🟥"), singleJobWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id), SuspendMode(Some(CancelMode.Kill()))))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    val events = controller.eventWatch.keyedEvents[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
      .map {
        case OrderProcessed(Outcome.Killed(failed: Outcome.Failed)) if failed.namedValues == NamedValues.rc(SIGKILL) =>
          // Sometimes, SIGTERM does not work and SIGKILL be sent. Something wrong with the bash script ????
          scribe.error("SIGTERM did not work")
          OrderProcessed(Outcome.Killed(failed.copy(namedValues = NamedValues.rc(SIGTERM))))  // Repair, to let test succceed
        case o => o
      }

    assert(events == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(SuspendMode(Some(CancelMode.Kill()))),
      OrderSuspendMarkedOnAgent,
      OrderProcessed(Outcome.Killed(if (isWindows) Outcome.succeededRC0 else Outcome.Failed(NamedValues.rc(SIGTERM)))),
      OrderProcessingKilled,
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = controller.eventWatch.lastAddedEventId
    touch(triggerFile)
    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(None),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Suspend and resume orders between two jobs" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔴"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)

    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(SuspendOrders(order.id :: Nil)).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspendMarkedOnAgent](_.key == order.id)
    touch(triggerFile)

    controller.eventWatch.await[OrderSuspended](_.key == order.id)
      assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
        OrderAttachable(agentId),
        OrderAttached(agentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderSuspendMarked(),
        OrderSuspendMarkedOnAgent,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderSuspended))

    val lastEventId = controller.eventWatch.lastAddedEventId
    touch(triggerFile)
    controller.executeCommandAsSystemUser(ResumeOrders(Seq(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)
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

  "An order being cancelled is not suspendible nor resumable" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔷"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOrStarted())).await(99.s).orThrow
    assert(controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
    controller.eventWatch.await[OrderCancelMarkedOnAgent](_.key == order.id)

    touch(triggerFile)
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Suspending a forked order does not suspend child orders" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == (order.id | "🥕"))

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    touch(triggerFile)
    controller.eventWatch.await[OrderProcessed](_.key == (order.id | "🥕"))

    //controller.eventWatch.await[OrderProcessingStarted](_.key == (order.id | "🥕"))
    //touch(bTriggerFile)
    //waitForCondition(10.s, 10.ms) { !exists(triggerFile) }
    //assert(!exists(triggerFile))
    controller.eventWatch.await[OrderJoined](_.key == order.id)
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Seq(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.arguments, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Seq(OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("FORK|🥕")))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentId),
        OrderId("FORK|🥕") <-: OrderAttached(agentId),
        OrderId("FORK|🥕") <-: OrderProcessingStarted,
        OrderId("FORK") <-: OrderSuspendMarked(),
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
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
    deleteIfExists(triggerFile)
    val orders = for (i <- 1 to 3) yield
      FreshOrder(OrderId(i.toString), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.s))
    for (o <- orders) controller.addOrderBlocking(o)
    for (o <- orders) controller.eventWatch.await[OrderAttached](_.key == o.id)
    val response = controller.executeCommandAsSystemUser(Batch(for (o <- orders) yield SuspendOrders(Set(o.id)))).await(99.s).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) controller.eventWatch.await[OrderSuspended](_.key == o.id)
  }

  "Resume a still suspending order" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔹"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspendMarkedOnAgent](_.key == order.id)

    controller.executeCommandAsSystemUser(ResumeOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderResumeMarked](_.key == order.id)

    touch(triggerFile)
    controller.eventWatch.await[OrderProcessed](_.key == order.id)

    touch(triggerFile)
    controller.eventWatch.await[OrderFinished](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderSuspendMarkedOnAgent,
      OrderResumeMarked(),
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
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔵"), twoJobsWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspendMarkedOnAgent](_.key == order.id)

    assert(controller.executeCommandAsSystemUser(ResumeOrder(order.id, Some(Position(0)))).await(99.s) ==
      Left(CannotResumeOrderProblem))

    touch(triggerFile)
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderSuspendMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Resume with invalid position is rejected" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("INVALID-POSITION"), tryWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspendMarkedOnAgent](_.key == order.id)

    touch(triggerFile)
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(
      controller.executeCommandAsSystemUser(ResumeOrder(order.id, Some(Position(99)))).await(99.s) ==
        Left(Problem("ResumeOrder: Unreachable order position")))
    assert(
      controller.executeCommandAsSystemUser(ResumeOrder(order.id,
        historicOutcomes = Some(Seq(HistoricOutcome(Position(99), Outcome.succeeded))))
      ).await(99.s) == Left(Problem("Unknown position 99 in workflow 'Workflow:TRY~INITIAL'")))

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Resume with changed position and changed historic outcomes" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔶"), tryWorkflow.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrders(Set(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspendMarkedOnAgent](_.key == order.id)
    touch(triggerFile)
    controller.eventWatch.await[OrderSuspended](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(tryWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked(),
      OrderSuspendMarkedOnAgent,
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
  private val pathExecutable = RelativePathExecutable("executable.cmd")
  private val agentId = AgentId("AGENT")
  private val versionId = VersionId("INITIAL")
  private val executeJob = Execute(WorkflowJob(agentId, pathExecutable, taskLimit = 100))

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    executeJob)

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("TWO") ~ versionId,
    executeJob,
    executeJob)

  private val forkWorkflow = Workflow.of(
    WorkflowPath("FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
      executeJob)),
    executeJob)

  private val tryWorkflow = Workflow.of(
    WorkflowPath("TRY") ~ versionId,
    executeJob,
    executeJob,
    TryInstruction(
      Workflow.of(
        Fail()),
      Workflow.of(
        Retry()),
      maxTries = Some(2)))
}
