package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.LockExecutorTest.*
import js7.data.job.PathExecutable
import js7.data.lock.{Acquired, Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{LockDemand, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased}
import js7.data.order.{Order, OrderId}
import js7.data.state.ControllerTestStateView
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, LockInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}

final class LockExecutorTest extends OurTestSuite:

  private lazy val stateView = ControllerTestStateView.of(
    orders = Some(Seq(freeLockOrder, freeLockedOrder, occupiedLockOrder)),
    workflows = Some(Seq(workflow)),
    itemStates = Seq(
      LockState(freeLock),
      LockState(occupiedLock, Acquired.Exclusive(OrderId("OCCUPANT"))),
      LockState(exclusiveLock)))

  private lazy val service = new InstructionExecutorService(WallClock)

  "Lock acquired" in:
    check(freeLockOrder, OrderLocksAcquired(List(LockDemand(freeLock.path, count = Some(3)))))

  "Lock released" in:
    check(freeLockedOrder, OrderLocksReleased(List(freeLock.path)))

  "Acquire too much" in:
    val order = freeLockOrder.withPosition(Position(1))
    assert(service.toEvents(workflow.instruction(order.position), order, stateView) ==
      Left(Problem("Cannot fulfill lock count=4 with Lock:FREE-LOCK limit=3")))

  "Lock cannot acquired and is queued" in:
    check(occupiedLockOrder, OrderLocksQueued(List(LockDemand(occupiedLock.path, None))))

  "Lock released and waiting order continues" in:
    check(freeLockedOrder, OrderLocksReleased(List(freeLock.path)))

  "Exclusive Lock acquired" in:
    check(exclusiveLockOrder, OrderLocksAcquired(List(LockDemand(exclusiveLock.path))))

  "Exclusive Lock released" in:
    check(freeExclusiveLockedOrder, OrderLocksReleased(List(exclusiveLock.path)))

  "Multiple locks, not available" in:
    val order = Order(
      OrderId("MULTIPLE-NOT-AVAILABLE"),
      workflow.id /: Position(4),
      Order.Ready())
    assert(service.toEvents(workflow.instruction(order.position), order, stateView) ==
      Right(Seq(
        order.id <-: OrderLocksQueued(List(
          LockDemand(exclusiveLock.path, None),
          LockDemand(occupiedLock.path, None))))))

  "Multiple locks acquired" in:
    val order = Order(
      OrderId("MULTIPLE"),
      workflow.id /: Position(5),
      Order.Ready())
    assert(service.toEvents(workflow.instruction(order.position), order, stateView) ==
      Right(Seq(
        order.id <-: OrderLocksAcquired(List(
          LockDemand(exclusiveLock.path),
          LockDemand(freeLock.path))))))

  "Multiple locks, releases" in:
    val order = Order(
      OrderId("MULTIPLE"),
      workflow.id /: (Position(5) / BranchId.Lock % 1),
      Order.Ready())
    assert(service.toEvents(workflow.instruction(order.position), order, stateView) ==
      Right(Seq(
        order.id <-: OrderLocksReleased(List(
          exclusiveLock.path,
          freeLock.path)))))

  private def check(order: Order[Order.State], event: OrderLockEvent): Unit =
    assert(service.toEvents(workflow.instruction(order.position), order, stateView) ==
      Right(Seq(order.id <-: event)))


object LockExecutorTest:
  private val freeLock = Lock(LockPath("FREE-LOCK"), limit = 3)
  private val occupiedLock = Lock(LockPath("OCCUPIED-LOCK"), limit = 3)
  private val exclusiveLock = Lock(LockPath("EXCLUSIVE-LOCK"))

  private val execute = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("JOB")))

  private val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION",
    LockInstruction.single(freeLock.path, Some(3), Workflow.of(execute)),
    LockInstruction.single(freeLock.path, Some(4), Workflow.of(execute)),
    LockInstruction.single(occupiedLock.path, None, Workflow.of(execute)),
    LockInstruction.single(exclusiveLock.path, None, Workflow.of(execute)),
    LockInstruction.checked(
      List(
        LockDemand(exclusiveLock.path, None),
        LockDemand(occupiedLock.path, None)),
      Workflow.of(execute)
    ).orThrow,
    LockInstruction.checked(
      List(
        LockDemand(exclusiveLock.path, None),
        LockDemand(freeLock.path, None)),
      Workflow.of(execute)
    ).orThrow)

  private val freeLockOrder = Order(
    OrderId("ORDER-A"),
    workflow.id /: Position(0),
    Order.Ready())

  private val freeLockedOrder = Order(
    OrderId("ORDER-B"),
    workflow.id /: (Position(0) / BranchId.Lock % 1),
    Order.Ready())

  private val occupiedLockOrder = Order(
    OrderId("ORDER-C"),
    workflow.id /: Position(2),
    Order.Ready())

  private val exclusiveLockOrder = Order(
    OrderId("ORDER-D"),
    workflow.id /: Position(3),
    Order.Ready())

  private val freeExclusiveLockedOrder = Order(
    OrderId("ORDER-D"),
    workflow.id /: (Position(3) / BranchId.Lock % 1),
    Order.Ready())
