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
import js7.data.lock.Acquired.{Available, NonExclusive}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.{LockDemand, OrderAdded, OrderDeleted, OrderFailed, OrderFinished, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderMoved, OrderOutcomeAdded, OrderPromptAnswered, OrderPrompted, OrderStarted}
import js7.data.order.{Order, OrderEvent, OrderId, OrderOutcome}
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

  private val controllerState = ControllerState.forTest(
    workflows = Seq(workflow))
  private val orderId = OrderId("ORDER")

  "Lock acquired" in:
    val use = 2
    val (lock, workflow) = makeLockAndWorkflow(limit = 3, use = Some(use))

    /// OrderLocksAcquired ///

    var coll = nextEvents(orderId):
      initialControllerEventColl(lock, workflow, orderId)
    assert:
      coll.keyedEvents.toList == List(
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
      coll.keyedEvents.toList == List(
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
      coll.keyedEvents.toList == List(
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
      coll.keyedEvents.toList == List(
        orderId <-: OrderLocksReleased(List(lock.path)),
        orderId <-: OrderFinished(),
        orderId <-: OrderDeleted)
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Available, queue = Vector(bOrderId))

    /// bOrderId OrderLocksAcquired ///

    coll = nextEvents(bOrderId)(coll)
    assert:
      coll.keyedEvents.toList == List(
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
      coll.keyedEvents.toList == List(
        bOrderId <-: OrderLocksReleased(List(lock.path)),
        bOrderId <-: OrderFinished(),
        bOrderId <-: OrderDeleted)
    assert:
      coll.aggregate.keyTo(LockState)(lock.path) ==
        LockState(lock, acquired = Available)

  "Exclusive Lock acquired" in:
    pending // FIXME
    check(exclusiveLockOrder.id, OrderLocksAcquired(List(LockDemand(exclusiveLock.path))))

  "Exclusive Lock released" in:
    pending // FIXME
    check(freeExclusiveLockedOrder.id, OrderLocksReleased(List(exclusiveLock.path)))

  "Multiple locks, not available" in:
    pending // FIXME
    val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "1",
      LockInstruction.checked(
        List(
          LockDemand(exclusiveLock.path, None),
          LockDemand(occupiedLock.path, None)),
        Workflow.of:
          Prompt(expr"'PROMPT'")
      ).orThrow)

    var coll = initialControllerEventColl(Seq(exclusiveLock, occupiedLock), Seq(workflow))
    val order = Order(
      OrderId("MULTIPLE-NOT-AVAILABLE"),
      workflow.id /: Position(4),
      Order.Ready())
    assert:
      InstructionExecutor.testToEvents(order.id, controllerState) ==
        Right(Seq(
          order.id <-: OrderLocksQueued(List(
            LockDemand(exclusiveLock.path, None),
            LockDemand(occupiedLock.path, None)))))

  "Multiple locks acquired" in:
    pending // FIXME
    val order = Order(
      OrderId("MULTIPLE"),
      workflow.id /: Position(5),
      Order.Ready())
    assert:
      InstructionExecutor.testToEvents(order.id, controllerState) ==
        Right(Seq(
          order.id <-: OrderLocksAcquired(List(
            LockDemand(exclusiveLock.path),
            LockDemand(freeLock.path)))))

  "Multiple locks, releases" in:
    pending // FIXME
    val order = Order(
      OrderId("MULTIPLE"),
      workflow.id /: (Position(5) / BranchId.Lock % 1),
      Order.Ready())
    assert:
      InstructionExecutor.testToEvents(order.id, controllerState) ==
        Right(Seq(
          order.id <-: OrderLocksReleased(List(
            exclusiveLock.path,
            freeLock.path))))

  private def check(orderId: OrderId, event: OrderLockEvent): Unit =
    val checked = InstructionExecutor.toEventCalc(orderId)
      .calculate(EventColl(controllerState, Timestamp.now))
      .map: coll =>
        //??? controllerState = coll.aggregate
        coll.keyedEvents.toList
    assert(checked == Right(Seq(orderId <-: event)))

  private def check2(orderId: OrderId, events: OrderLockEvent*)
  : Checked[List[KeyedEvent[OrderEvent.OrderCoreEvent]]] =
    //val coll = InstructionExecutor.toEventCalc(order.id).calculateEvents(controllerState, Timestamp.now)
    val myControllerState = controllerState.applyKeyedEvents(events.map(orderId <-: _)).orThrow
    //InstructionExecutor.testToEvents(orderId, myControllerState)
    InstructionExecutor.toEventCalc(orderId)
      .calculate(EventColl(myControllerState, Timestamp.now))
      .map: coll =>
        //??? controllerState = coll.aggregate
        coll.keyedEvents.toList

object LockExecutorTest:
  private val freeLock = Lock(LockPath("FREE-LOCK"), limit = 3)
  private val occupiedLock = Lock(LockPath("OCCUPIED-LOCK"), limit = 3)
  private val exclusiveLock = Lock(LockPath("EXCLUSIVE-LOCK"))

  //private val execute = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("JOB")))
  private val prompt = Prompt(expr"'PROMPT'")

  private val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION",
    LockInstruction.single(freeLock.path, Some(3)):
      Workflow.of:
        prompt,
    LockInstruction.single(freeLock.path, Some(4)):
      Workflow.of:
        prompt,
    LockInstruction.single(occupiedLock.path, None):
      Workflow.of:
        prompt,
    LockInstruction.single(exclusiveLock.path, None):
      Workflow.of:
        prompt,
    LockInstruction.checked(
      List(
        LockDemand(exclusiveLock.path, None),
        LockDemand(occupiedLock.path, None)),
      Workflow.of(prompt)
    ).orThrow,
    LockInstruction.checked(
      List(
        LockDemand(exclusiveLock.path, None),
        LockDemand(freeLock.path, None)),
      Workflow.of(prompt)
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


  //private def applyControllerEvents(workflow: Workflow, lock: Lock, orderId: OrderId)
  //: ControllerEventColl =
  //  val initialColl = initialControllerEventColl(workflow, lock, orderId)
  //  nextEvents(initialColl, orderId)

  private def makeLockAndWorkflow(limit: Int, use: Option[Int]): (Lock, Workflow) =
    val lock = makeLock(limit)
    val workflow = makeWorkflow(lock.path, use = use)
    lock -> workflow

  private def makeLock(limit: Int): Lock =
    Lock(LockPath("LOCK"), limit = limit, itemRevision = Some(ItemRevision(1)))

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
      OrderEventSource.nextEvents(orderId).addTo(coll) match
        case Left(problem) => Left(problem)
        case Right(result) =>
          if result.hasMoreEventsThan(coll) then
            if i == 100 then fail(s"Endless loop in nextEvents for order $orderId?")
            loop(result, i + 1)
          else
            Right(result)
    loop(coll.forward, 1)

  private def makeControllerEventColl(workflows: Seq[Workflow]): ControllerEventColl =
    ControllerEventColl(
      ControllerState.forTest(workflows = workflows),
      Timestamp.now)
