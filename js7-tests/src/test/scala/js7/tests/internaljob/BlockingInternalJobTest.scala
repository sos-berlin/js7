package js7.tests.internaljob

import java.util.concurrent.locks.ReentrantLock
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.time.{Timestamp, WaitForCondition}
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.data_for_java.order.JOutcome
import js7.launcher.forjava.internal.BlockingInternalJob
import js7.launcher.forjava.internal.BlockingInternalJob.{OrderProcess, Step}
import js7.tests.internaljob.BlockingInternalJobTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest, DirectoryProvider}
import scala.concurrent.duration.*

final class BlockingInternalJobTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.delay = 10ms
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Throwing BlockingInternalJob" in:
    val workflow = Workflow.of(WorkflowPath("THROWING-JOB"),
      Execute(WorkflowJob(
        agentPath,
        InternalExecutable(classOf[ThrowingJob].getName))))
    withTemporaryItem(workflow): workflow =>
      val orderId = OrderId("THROWING-JOB")

      val events = controller.runOrder:
        FreshOrder(orderId, workflow.path)

      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("TEST-EXCEPTION"))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))

  "Cancellation" - {
    "Cancellation of a BlockingInternalJob is ignored" in:
      // ...because our NonInterruptibleJob does not implement cancellation
      val workflow = Workflow.of(WorkflowPath("BLOCKING-JOB"),
        Execute(WorkflowJob(
          agentPath,
          InternalExecutable(classOf[NonInterruptibleJob].getName))))
      withTemporaryItem(workflow): workflow =>
        val orderId = OrderId("BLOCKING-JOB")

        NonInterruptibleJob.lock.lock()
        controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderStdoutWritten](_.key == orderId)

        waitForCondition(10.s, 10.ms):
          NonInterruptibleJob.lock.isLocked

        execCmd(CancelOrders(Seq(orderId), CancellationMode.kill(immediately = true)))
        sleep(1.s)

        assert(controllerState.idToOrder(orderId).isState[Order.Processing])
        NonInterruptibleJob.lock.unlock()
        eventWatch.awaitNext[OrderTerminated](_.key == orderId).head.value.event

    "Cancellation of a BlockingInternalJob using InterruptibleOrderProcess, not catching InterruptedException" in:
      // InterruptedException is special, because NonFatal considers it as fatal
      val workflow = Workflow.of(WorkflowPath("THROWING-INTERRUPTIBLE-BLOCKING-JOB"),
        Execute(WorkflowJob(
          agentPath,
          InternalExecutable(classOf[ThrowingInteruptibleJob].getName))))
      withTemporaryItem(workflow): workflow =>
        val orderId = OrderId("THROWING-INTERRUPTIBLE-BLOCKING-JOB")

        ThrowingInteruptibleJob.lock.lock()
        controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderStdoutWritten](_.key == orderId)

        waitForCondition(10.s, 10.ms):
          ThrowingInteruptibleJob.lock.isLocked

        execCmd(CancelOrders(Seq(orderId), CancellationMode.kill()))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId).head.value.event

        ThrowingInteruptibleJob.lock.unlock()
        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("ThrowingInteruptibleJob\n"),
          OrderCancellationMarked(CancellationMode.kill()),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(OrderOutcome.Failed(Some("java.lang.InterruptedException")))),
          OrderProcessingKilled,
          OrderDetachable,
          OrderDetached,
          OrderCancelled,
          OrderDeleted))

    "Cancellation of a BlockingInternalJob using InterruptibleOrderProcess" in:
      // (Cancellation may be implemented via cancel method) */
      val workflow = Workflow.of(WorkflowPath("INTERRUPTIBLE-BLOCKING-JOB"),
        Execute(WorkflowJob(
          agentPath,
          InternalExecutable(classOf[InteruptibleJob].getName))))
      withTemporaryItem(workflow): workflow =>
        val orderId = OrderId("INTERRUPTIBLE-BLOCKING-JOB")

        InteruptibleJob.lock.lock()
        controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderStdoutWritten](_.key == orderId)

        waitForCondition(10.s, 10.ms):
          InteruptibleJob.lock.isLocked

        execCmd(CancelOrders(Seq(orderId), CancellationMode.kill()))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId).head.value.event

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("InteruptibleJob\n"),
          OrderCancellationMarked(CancellationMode.kill()),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(OrderOutcome.Failed(Some("java.lang.InterruptedException")))),
          OrderProcessingKilled,
          OrderDetachable,
          OrderDetached,
          OrderCancelled,
          OrderDeleted))
  }


object BlockingInternalJobTest:

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  final class ThrowingJob extends BlockingInternalJob:
    def toOrderProcess(step: Step) = () =>
      throw new RuntimeException("TEST-EXCEPTION")


  /** This standard job ignores cancellation. */
  final class NonInterruptibleJob extends BlockingInternalJob:
    def toOrderProcess(step: Step) =
      () =>
        step.out.println("NonInterruptibleJob")
        NonInterruptibleJob.lock.lockInterruptibly()
        JOutcome.succeeded

  object NonInterruptibleJob:
    val lock = ReentrantLock()


  final class ThrowingInteruptibleJob extends BlockingInternalJob:
    def toOrderProcess(step: Step) =
      var thread: Thread | Null = null
      new OrderProcess:
        def run() =
          thread = Thread.currentThread()
          step.out.println("ThrowingInteruptibleJob")
          ThrowingInteruptibleJob.lock.lockInterruptibly()
          JOutcome.succeeded

        override def cancel(immediately: Boolean) =
          thread.interrupt()

  object ThrowingInteruptibleJob:
    val lock = ReentrantLock()


  final class InteruptibleJob extends BlockingInternalJob:
    def toOrderProcess(step: Step): BlockingInternalJob.InterruptibleOrderProcess =
      () =>
        step.out.println("InteruptibleJob")
        InteruptibleJob.lock.lockInterruptibly()
        JOutcome.succeeded

  object InteruptibleJob:
    val lock = ReentrantLock()