package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.controller.{ControllerEventColl, ControllerState}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventColl, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.LockExecutorTest.*
import js7.data.item.ItemRevision
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.lock.Acquired.{Available, Exclusive, NonExclusive}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{LockDemand, OrderAdded, OrderDeleted, OrderFailed, OrderFinished, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderMoved, OrderOutcomeAdded, OrderPromptAnswered, OrderPrompted, OrderStarted}
import js7.data.order.{OrderEvent, OrderId, OrderOutcome}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.{LockInstruction, Prompt}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import org.scalatest.Assertions.fail
import scala.annotation.tailrec
import scala.language.implicitConversions

final class LockExecutorTest extends OurTestSuite:

  private val orderId = OrderId("ORDER")

  "Lock acquired" in:
    val use = 2
    val (lock, workflow) = makeLockAndWorkflow(limit = 3, use = Some(use))

    /// OrderLocksAcquired ///

    var coll = nextEvents(orderId):
      initialControllerEventColl(lock, workflow, orderId)
    assert:
      coll.keyedEventList == List(
        orderId <-: OrderStarted,
        orderId <-: OrderLocksAcquired(List(LockDemand(lock.path, count = Some(use)))),
        orderId <-: OrderPrompted("PROMPT"))
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = NonExclusive(Map(orderId -> use)))

    /// OrderLocksReleased ///

    coll = nextEvents(orderId):
      coll.add(
        orderId <-: OrderPromptAnswered(),
        orderId <-: OrderMoved(Position(0) / "lock" % 1))
      .orThrow
    assert:
      coll.keyedEventList == List(
        orderId <-: OrderLocksReleased(List(lock.path)),
        orderId <-: OrderFinished(),
        orderId <-: OrderDeleted)
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Available)

  "Acquire too much" in:
    val (lock, workflow) = makeLockAndWorkflow(limit = 3, use = Some(4))
    val coll = nextEvents(orderId):
      initialControllerEventColl(lock, workflow, orderId)
    assert:
      coll.keyedEventList == List(
        orderId <-: OrderOutcomeAdded:
          OrderOutcome.Disrupted:
            Problem("Cannot fulfill lock count=4 with Lock:LOCK limit=3"),
        orderId <-: OrderFailed(Position(0)))
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) == LockState(lock, acquired = Available)

  "Lock cannot be acquired and is queued" in:
    val use = 2
    val (lock, workflow) = makeLockAndWorkflow(limit = 3, use = Some(use))

    /// orderId OrderLocksAcquired ///

    var coll = nextEvents(orderId):
      initialControllerEventColl(lock, workflow, orderId)

    /// bOrderId OrderLocksQueued, bOrderId waits ///
    val bOrderId = OrderId("B-ORDER")

    coll = nextEvents(bOrderId):
      coll.add:
        bOrderId <-: OrderAdded(workflow.id, deleteWhenTerminated = true)
      .orThrow

    /// orderId OrderLocksReleased ///

    coll = nextEvents(orderId):
      coll.add(
          orderId <-: OrderPromptAnswered(),
          orderId <-: OrderMoved(Position(0) / "lock" % 1))
        .orThrow
    assert:
      coll.keyedEventList == List(
        orderId <-: OrderLocksReleased(List(lock.path)),
        orderId <-: OrderFinished(),
        orderId <-: OrderDeleted)
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Available, queue = Vector(bOrderId))

    /// bOrderId OrderLocksAcquired ///

    coll = nextEvents(bOrderId)(coll)
    assert:
      coll.keyedEventList == List(
        bOrderId <-: OrderLocksAcquired(List(LockDemand(lock.path, count = Some(use)))),
        bOrderId <-: OrderPrompted("PROMPT"))
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = NonExclusive(Map(bOrderId -> use)))

    /// bOrderId OrderLocksReleased, waiting bOrderId continues ///

    coll = nextEvents(bOrderId):
      coll.add(
          bOrderId <-: OrderPromptAnswered(),
          bOrderId <-: OrderMoved(Position(0) / "lock" % 1))
        .orThrow
    assert:
      coll.keyedEventList == List(
        bOrderId <-: OrderLocksReleased(List(lock.path)),
        bOrderId <-: OrderFinished(),
        bOrderId <-: OrderDeleted)
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Available)

  "Exclusive Lock acquired and released" in:
    val (lock, workflow) = makeLockAndWorkflow(limit = 3, use = None/*exclusive*/)

    /// OrderLocksAcquired ///

    var coll = nextEvents(orderId):
      initialControllerEventColl(lock, workflow, orderId)
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Exclusive(orderId))

    /// OrderLocksReleased ///

    coll = nextEvents(orderId):
      coll.add(
          orderId <-: OrderPromptAnswered(),
          orderId <-: OrderMoved(Position(0) / "lock" % 1))
        .orThrow
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Available)

  "Multiple locks" in:
    val aOrderId = OrderId("A-ORDER")
    val bOrderId = OrderId("B-ORDER")
    val aLock = makeLock(LockPath("A-LOCK"), limit = 3)
    val bLock = makeLock(LockPath("B-LOCK"))

    val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "1",
      LockInstruction(
        List(
          LockDemand(aLock.path),
          LockDemand(bLock.path))
      ):
        Workflow.of:
          Prompt(expr"'PROMPT'"))

    var coll = initialControllerEventColl(Seq(bLock, aLock), Seq(workflow))
    coll = addOrder(coll, aOrderId, workflow.id)
    coll = checkedNextEvents(aOrderId)(coll).orThrow

    assert:
      coll.keyedEventList == Seq(
        aOrderId <-: OrderStarted,
        aOrderId <-: OrderLocksAcquired(List(
          LockDemand(aLock.path),
          LockDemand(bLock.path))),
        aOrderId <-: OrderPrompted("PROMPT"))

    coll = addOrder(coll, bOrderId, workflow.id)
    coll = nextEvents(bOrderId)(coll)
    assert:
      coll.keyedEventList == Seq(
        bOrderId <-: OrderStarted,
        bOrderId <-: OrderLocksQueued(List(
          LockDemand(aLock.path),
          LockDemand(bLock.path))))

  private def check(orderId: OrderId, events: OrderLockEvent*)
  : Checked[List[KeyedEvent[OrderEvent.OrderCoreEvent]]] =
    val myControllerState = ControllerState.empty.applyKeyedEvents(events.map(orderId <-: _))
      .orThrow
    InstructionExecutor.toEventCalc(orderId)
      .calculate(EventColl(myControllerState, Timestamp.now))
      .map: coll =>
        coll.keyedEventList


object LockExecutorTest:


  private def makeLockAndWorkflow(limit: Int, use: Option[Int]): (Lock, Workflow) =
    val lock = makeLock(limit = limit)
    val workflow = makeWorkflow(lock.path, use = use)
    lock -> workflow

  private def makeLock(lockPath: LockPath = LockPath("LOCK"), limit: Int = 1) =
    Lock(lockPath, limit = limit, itemRevision = Some(ItemRevision(1)))

  private def makeWorkflow(lockPath: LockPath, use: Option[Int]): Workflow =
    Workflow.of(WorkflowPath("WORKFLOW") ~ "1",
      LockInstruction.single(lockPath, count = use):
        Workflow.of:
          Prompt(expr"'PROMPT'"))

  private def initialControllerEventColl(lock: Lock, workflow: Workflow, orderId: OrderId)
  : ControllerEventColl =
    val coll = initialControllerEventColl(Seq(lock), Seq(workflow))
    addOrder(coll, orderId, workflow.id)

  private def initialControllerEventColl(locks: Seq[Lock], workflows: Seq[Workflow])
  : ControllerEventColl =
    makeControllerEventColl(workflows)
      .add:
        locks.map: lock =>
          NoKey <-: UnsignedSimpleItemAdded(lock)
      .orThrow

  private def addOrder(coll: ControllerEventColl, orderId: OrderId, workflowId: WorkflowId): ControllerEventColl =
    coll.add:
      orderId <-: OrderAdded(workflowId, deleteWhenTerminated = true)
    .orThrow

  private def nextEvents(orderId: OrderId)(coll: ControllerEventColl): ControllerEventColl =
    checkedNextEvents(orderId)(coll).orThrow

  private def checkedNextEvents(orderId: OrderId)(coll: ControllerEventColl): Checked[ControllerEventColl] =
    @tailrec def loop(coll: ControllerEventColl, i: Int): Checked[ControllerEventColl] =
      OrderEventSource.nextStepEvents(orderId).addTo(coll) match
        case Left(problem) => Left(problem)
        case Right(result) =>
          if result.hasMoreEventsThan(coll) then
            if i == 100 then fail(s"Endless loop in checkedNextEvents for order $orderId?")
            loop(result, i + 1)
          else
            Right(result)
    loop(coll.forward, 1)

  private def makeControllerEventColl(workflows: Seq[Workflow]): ControllerEventColl =
    ControllerEventColl(
      ControllerState.forTest(workflows = workflows),
      Timestamp.now)
