package js7.data.controller

import cats.effect.unsafe.IORuntime
import js7.base.crypt.silly.SillySigner
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem, UnknownItemPathProblem}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerStateExecutorTest.*
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{AnyKeyedEvent, Event, EventCalc, EventColl, KeyedEvent, TimeCtx}
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged, VersionedItemRemoved}
import js7.data.item.{ItemRevision, ItemSigner, VersionId}
import js7.data.job.{InternalExecutable, JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{LockDemand, OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderLocksAcquired, OrderMoved, OrderStarted}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.plan.PlanSchema
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.expression.Expression.{StringConstant, expr}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.OrderParameterList.{MissingOrderArgumentProblem, WrongValueTypeProblem}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, LockInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import org.scalatest.Assertions.assert
import scala.collection.View

final class ControllerStateExecutorTest extends OurTestSuite:

  import ControllerStateExecutorTest.itemSigner.sign

  private given IORuntime = ioRuntime

  "resetAgent" in:
    pending // TODO

  private lazy val stdVerifiedUpdateItems = VerifiedUpdateItems(
    VerifiedUpdateItems.Simple(
      Seq(
        aSubagentItem.withRevision(None),
        bSubagentItem.withRevision(None),
        aAgentRef.withRevision(None),
        bAgentRef.withRevision(None),
        lock.withRevision(None)),
      Seq(
        Verified(sign(aJobResource.withRevision(None)), Nil),
        Verified(sign(bJobResource.withRevision(None)), Nil)),
      delete = Nil),
    Some(VerifiedUpdateItems.Versioned(
      v1,
      Seq(
        Verified(sign(aWorkflow), Nil),
        Verified(sign(bWorkflow), Nil)),
      remove = Nil)))

  private lazy val stdItems =
    Set(
      aSubagentItem, aAgentRef,
      bSubagentItem, bAgentRef,
      lock,
      aJobResource, bJobResource,
      aWorkflow, bWorkflow,
      PlanSchema.Global)

  "stdVerifiedUpdateItems" in:
    val executor = Executor()
    executor.executeVerifiedUpdateItems(stdVerifiedUpdateItems).orThrow
    assert(executor.controllerState.keyToItem.values.toSet == stdItems)

  "executeVerifiedUpdateItems" - {
    "Add workflow but referenced items are missing" in:
      val executor = Executor()
      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Nil, Nil, Nil),
          Some(VerifiedUpdateItems.Versioned(
            v1,
            Seq(
              Verified(sign(aWorkflow), Nil),
              Verified(sign(bWorkflow), Nil)),
            remove = Nil))
        )) == Left(Problem.Combined(Set(
          MissingReferencedItemProblem(aWorkflow.id, aAgentRef.path),
          MissingReferencedItemProblem(bWorkflow.id, bAgentRef.path),
          MissingReferencedItemProblem(bWorkflow.id, lock.path),
          MissingReferencedItemProblem(bWorkflow.id, aJobResource.path),
          MissingReferencedItemProblem(bWorkflow.id, bJobResource.path)))))

    "Delete AgentRef but it is in use by a workflow" in:
      val executor = Executor()

      val firstEvents = executor.executeVerifiedUpdateItems(stdVerifiedUpdateItems)
        .orThrow

      assert:
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aSubagentId, aAgentRef.path))
        )) == Left(ItemIsStillReferencedProblem(aAgentRef.path, aWorkflow.id))

      assert:
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aSubagentId, aAgentRef.path)),
          Some(VerifiedUpdateItems.Versioned(v2, remove = Seq(aWorkflow.path)))
        )) == Right(
          Seq[AnyKeyedEvent](
            ItemDeleted(aSubagentId),
            ItemDeleted(aAgentRef.path),
            VersionAdded(v2),
            VersionedItemRemoved(aWorkflow.path),
            ItemDeleted(aWorkflow.id)))

    "Delete AgentRef, but it is in use by a deleted workflow still containing orders" in:
      val executor = Executor()

      executor.executeVerifiedUpdateItems(stdVerifiedUpdateItems)
        .orThrow

      val orderId = OrderId("ORDER")
      executor.applyEvents:
        orderId <-: OrderAdded(aWorkflow.id)

      executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
        VerifiedUpdateItems.Simple(),
        Some(VerifiedUpdateItems.Versioned(VersionId("2"), remove = Seq(aWorkflow.path)))
      )).orThrow

      assert:
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aSubagentId, aAgentRef.path))
        )) == Left(ItemIsStillReferencedProblem(aAgentRef.path, aWorkflow.id, " with Order:ORDER"))

      executor
        .applyWithSubsequentEvents(Seq(
          //??? orderId <-: OrderDetached, // was attachable, we force it to be detached
          orderId <-: OrderCancelled,
          orderId <-: OrderDeleted))
        .orThrow

      val firstEvents = executor.coll.keyedEvents
      assert:
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aSubagentId, aAgentRef.path))
        )) == Right(firstEvents ++
          // subsequent events
          Seq(
            NoKey <-: ItemDeleted(aSubagentId),
            NoKey <-: ItemDeleted(aAgentRef.path)))

    // TODO Don't use ControllerStateTest here
    import ControllerStateTest.{controllerState, fileWatch, workflow}

    "Empty" in:
      val executor = Executor(controllerState)
      executor
        .executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(),
          None))
        .orThrow

    "Delete and add fileWatch" in:
      val executor = Executor(controllerState)

      // Delete the fileWatch
      assert(executor.controllerState.keyToItem.contains(fileWatch.path))
      executor
        .executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(fileWatch.path))))
        .orThrow
      assert(!executor.controllerState.keyToItem.contains(fileWatch.path))

      locally:
        // Delete the workflow
        val deletedWorkflow = Executor(executor.controllerState)
        deletedWorkflow
          .executeVerifiedUpdateItems(VerifiedUpdateItems(
            VerifiedUpdateItems.Simple(),
            Some(VerifiedUpdateItems.Versioned(VersionId("x"), remove = Seq(workflow.path)))))
          .orThrow
        // The deleted workflow still contains an order and has not been deleted
        assert(deletedWorkflow.controllerState.keyToItem.contains(workflow.id))

        // FileWatch requires a non-deleted workflow
        assert(deletedWorkflow
          .executeVerifiedUpdateItems(VerifiedUpdateItems(
            VerifiedUpdateItems.Simple(Seq(fileWatch.withRevision(None)))
          )) == Left(MissingReferencedItemProblem(fileWatch.path, workflow.path)))

      // Add the workflow
      executor
        .executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Seq(fileWatch.withRevision(None)))))
        .orThrow

      assert(executor.controllerState.keyToItem.contains(fileWatch.path))
  }

  "addOrders" - {
    lazy val aOrderId = OrderId("A")
    lazy val bOrderId = OrderId("B")
    lazy val executor = Executor()

    "addOrder for unknown workflow is rejected" in:
      assert:
        ControllerStateExecutor
          .addOrders:
            Seq(FreshOrder(aOrderId, aWorkflow.path))
          .calculate(executor.coll) == Left(UnknownItemPathProblem(aWorkflow.path))

    "addOrder without a required argument is rejected" in:
      executor.executeVerifiedUpdateItems(stdVerifiedUpdateItems).orThrow
      assert:
        executor.execute(ControllerStateExecutor.addOrders(Seq(
          FreshOrder(aOrderId, aWorkflow.path),
          FreshOrder(bOrderId, bWorkflow.path)
        ))) ==
          Left(MissingOrderArgumentProblem(requiredParameter))

    "addOrder with a wrong argument type is rejected" in:
      assert:
        executor.execute(ControllerStateExecutor.addOrders(Seq(
          FreshOrder(bOrderId, bWorkflow.path, Map("required" -> StringValue("STRING")))
        ))) == Left(WrongValueTypeProblem("required", StringValue, requiredParameter.valueType))

    "addOrder resolved default argument and provides variables" in:
      executor.execute:
        ControllerStateExecutor.addOrders(Seq(
          FreshOrder(aOrderId, aWorkflow.path),
          FreshOrder(bOrderId, bWorkflow.path, Map("required" -> NumberValue(7)))))
      .orThrow

      assert(executor.controllerState.idToOrder.values.toSet == Set(
        Order(aOrderId, aWorkflow.id /: Position(0), Order.Fresh(),
          attachedState = Some(Order.Attaching(aAgentRef.path))),
        Order(bOrderId, bWorkflow.id /: (Position(0) / "lock" % 0), Order.Ready(),
          arguments = Map(
            "required" -> NumberValue(7),
            "variable" -> StringValue("VARIABLE-VALUE")),
          attachedState = Some(Order.Attaching(bAgentRef.path)))))
  }

  "addSubsequentEvents" - {
    var _controllerState = ControllerState.empty
    var updated = ControllerState.empty

    def applyEvents(keyedEvents: KeyedEvent[Event]*) =
      val executor = Executor(_controllerState)
      val result = executor.applyWithSubsequentEvents:
        EventColl(_controllerState, TimeCtx(ts"2025-04-26T12:00:00Z"))
          .add(keyedEvents).orThrow.keyedEvents
      updated = executor.controllerState
      result

    "No events" in:
      assert(applyEvents() == Right(Nil))

    "Add agents, workflows and orders" in:
      assert(
        VerifiedUpdateItemsExecutor.execute(stdVerifiedUpdateItems, ControllerState.empty) ==
          Right(Seq[AnyKeyedEvent](
            NoKey <-: SignedItemAdded(sign(aJobResource)),
            NoKey <-: SignedItemAdded(sign(bJobResource)),
            NoKey <-: UnsignedSimpleItemAdded(aSubagentItem),
            NoKey <-: UnsignedSimpleItemAdded(bSubagentItem),
            NoKey <-: UnsignedSimpleItemAdded(aAgentRef),
            NoKey <-: UnsignedSimpleItemAdded(bAgentRef),
            NoKey <-: UnsignedSimpleItemAdded(lock),
            NoKey <-: VersionAdded(v1),
            NoKey <-: VersionedItemAdded(sign(aWorkflow)),
            NoKey <-: VersionedItemAdded(sign(bWorkflow)))))

      _controllerState = updated

    "After VersionedItemRemoved or VersionItemChanged, the unused workflows are deleted" - {
      val a2Workflow = aWorkflow.withVersion(v2)
      val a4Workflow = aWorkflow.withVersion(v4)
      lazy val executor = Executor()

      "v1 VersionItemAdded" in:
        val keyedEvents = executor
          .applyWithSubsequentEvents(
            Seq(
              NoKey <-: VersionAdded(v1),
              NoKey <-: VersionedItemAdded(sign(aWorkflow))))
          .orThrow
        assert(keyedEvents.isEmpty)

        assert(executor.toSnapshot == Seq(
          SnapshotEventId(0),
          VersionAdded(v1),
          VersionedItemAdded(sign(aWorkflow))))

      "v2 VersionItemChanged" in:
        val keyedEvents = executor.applyWithSubsequentEvents(Seq(
          VersionAdded(v2),
          VersionedItemChanged(sign(a2Workflow))))
        assert(keyedEvents == Right(Seq(
          NoKey <-: ItemDeleted(aWorkflow.id))))

        assert(executor.toSnapshot == Seq(
          SnapshotEventId(0),
          VersionAdded(v2),
          VersionedItemAdded(sign(a2Workflow))))

      "v3 VersionItemRemoved" in:
        val keyedEvents = executor.applyWithSubsequentEvents(Seq(
          VersionAdded(v3),
          VersionedItemRemoved(aWorkflow.path)))
        assert(keyedEvents == Right(Seq(
          NoKey <-: ItemDeleted(a2Workflow.id))))

        assert(executor.toSnapshot == Seq(
          SnapshotEventId(0)))

      "v4 VersionItemAdded" in:
        val keyedEvents =
          executor.applyWithSubsequentEvents:
            Seq(
              VersionAdded(v4),
              VersionedItemAdded(sign(a4Workflow)))

        assert(keyedEvents == Right(Nil))

        assert(executor.toSnapshot == Seq(
          SnapshotEventId(0),
          VersionAdded(v4),
          VersionedItemAdded(sign(a4Workflow))))

      "v5 VersionItemRemoved" in:
        val keyedEvents = executor.applyWithSubsequentEvents(Seq(
          VersionAdded(v5),
          VersionedItemRemoved(aWorkflow.path)))
        assert(keyedEvents == Right(Seq(
          NoKey <-: ItemDeleted(a4Workflow.id))))

        assert(executor.toSnapshot == Seq(
          SnapshotEventId(0)))
    }

    "Workflow is deleted after last OrderDeleted" in:
      val executor = Executor()

      executor.executeVerifiedUpdateItems(stdVerifiedUpdateItems)
        .orThrow

      assert(
        executor.applyWithSubsequentEvents(Seq(
          aOrderId <-: OrderAdded(aWorkflow.id),
          bOrderId <-: OrderAdded(bWorkflow.id)
        )) == Right(Seq(
          bOrderId <-: OrderStarted,
          aOrderId <-: OrderAttachable(aAgentRef.path),
          bOrderId <-: OrderLocksAcquired(List(LockDemand(lock.path))),
          bOrderId <-: OrderAttachable(bAgentRef.path))))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          VersionAdded(v2),
          VersionedItemRemoved(aWorkflow.path)
        )) == Right(Nil))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          NoKey <-: ItemAttached(aJobResource.path, Some(ItemRevision(0)), bAgentRef.path),
          NoKey <-: ItemAttached(aWorkflow.id, None, aAgentRef.path),
          NoKey <-: ItemAttached(bWorkflow.id, None, bAgentRef.path),
          aOrderId <-: OrderAttached(aAgentRef.path),
          bOrderId <-: OrderAttached(bAgentRef.path),
          aOrderId <-: OrderMoved(Position(1)),
          bOrderId <-: OrderMoved(Position(1))
        )) == Right(Nil))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          aOrderId <-: OrderDetachable,
          bOrderId <-: OrderDetachable
        )) == Right(Nil))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          aOrderId <-: OrderDetached,
          bOrderId <-: OrderDetached
        )) == Right(Seq(
          bOrderId <-: OrderFinished(),
          aOrderId <-: OrderStarted,
          aOrderId <-: OrderFinished())))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          aOrderId <-: OrderDeleted,
          bOrderId <-: OrderDeleted
        )) == Right(Seq(
          NoKey <-: ItemDetachable(aWorkflow.id, aAgentRef.path))))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          NoKey <-: ItemDetached(aWorkflow.id, aAgentRef.path)
        )) == Right(Seq(
          NoKey <-: ItemDeleted(aWorkflow.id))))

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Nil, Nil, Nil),
          Some(VerifiedUpdateItems.Versioned(
            v3,
            Nil,
            remove = Seq(bWorkflow.path)))
        )) == Right(Seq(
          NoKey <-: VersionAdded(v3),
          NoKey <-: VersionedItemRemoved(bWorkflow.path),
          NoKey <-: ItemDetachable(bWorkflow.id, bAgentRef.path))))

      assert(
        executor.applyWithSubsequentEvents(Seq(
          NoKey <-: ItemDetached(bWorkflow.id, bAgentRef.path)
        )) == Right(Seq(
          NoKey <-: ItemDeleted(bWorkflow.id))))

/*
    assert(
      applyKeyedEvents(_controllerState,
        aOrderId <-: OrderAdded(aWorkflow.id),
        bOrderId <-: OrderAdded(bWorkflow.id),
        cOrderId <-: OrderAdded(cWorkflow.id),
        aOrderId <-: OrderAttachable(aAgentRef.path),
        bOrderId <-: OrderAttachable(bAgentRef.path),
        cOrderId <-: OrderAttachable(cAgentPath),
        ItemAttached(aWorkflow.id, None, aAgentRef.path),
        ItemAttached(bWorkflow.id, None, bAgentRef.path),
        ItemAttached(cWorkflow.id, None, cAgentPath),
        aOrderId <-: OrderAttached(aAgentRef.path),
        bOrderId <-: OrderAttached(bAgentRef.path),
        cOrderId <-: OrderAttached(cAgentPath)
      ) == Right(Nil))

    assert(
      applyKeyedEvents(_controllerState,
        aOrderId <-: OrderDetachable,
        aOrderId <-: OrderDetached,
        bOrderId <-: OrderDetachable
      ) == Right(Seq(
        aOrderId <-: OrderStarted,
        aOrderId <-: OrderFinished)))

    assert(
      applyKeyedEvents(_controllerState,
        aOrderId <-: OrderDeletionMarked)
      == Right(Seq(
        aOrderId <-: OrderDeleted)))

    assert(applyKeyedEvents(_controllerState,
      VersionAdded(v2),
      VersionedItemRemoved(aWorkflow.path),
      VersionedItemRemoved(bWorkflow.path),
      VersionedItemRemoved(cWorkflow.path)
    ) == Right(Seq(
      NoKey <-: ItemDeleted(cWorkflow.id),
      NoKey <-: ItemDetachable(aWorkflow.id, aAgentRef.path))))

    assert(applyKeyedEvents(_controllerState,
      ItemDeletionMarked(aAgentRef.path)
    ) == Right(Nil))

    assert(applyKeyedEvents(_controllerState,
      ItemDeletionMarked(bAgentRef.path))
      == Right(Seq(
        NoKey <-: ItemDetachable(bWorkflow.id, bAgentRef.path))))

    assert(applyKeyedEvents(_controllerState,
      ItemDetached(aWorkflow.id, aAgentRef.path)
    ) == Right(Seq(
      NoKey <-: ItemDeleted(aAgentRef.path))))
 */
  }

  "nextOrderWatchOrderEvents" in:
    pending // TODO

  "nextOrderEvents" in:
    pending // TODO

  "TEST WIP" in:
    final case class X(controllerState: ControllerState, keyedEvents: Seq[AnyKeyedEvent] = Nil):
      def doSomething: Checked[X] =
        val keyedEvents = Seq(NoKey <-: VersionAdded(VersionId("TEST")))
        controllerState.applyKeyedEvents(keyedEvents)
          .map(X(_, keyedEvents))
    val controllerState = ControllerState.empty
    for
      x <- X(controllerState).doSomething
      x <- x.doSomething
    yield x


object ControllerStateExecutorTest:

  private val timeCtx = TimeCtx(ts"2025-04-29T12:00:00Z")
  private implicit val instructionExecutorService: InstructionExecutorService =
    new InstructionExecutorService(WallClock)

  private val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)

  private val aSubagentId = SubagentId("A-SUBAGENT")
  private val bSubagentId = SubagentId("B-SUBAGENT")
  private val aAgentRef = AgentRef(AgentPath("A-AGENT"), directors = Seq(aSubagentId),
    itemRevision = Some(ItemRevision(0)))
  private val bAgentRef = AgentRef(AgentPath("B-AGENT"), directors = Seq(bSubagentId),
    itemRevision = Some(ItemRevision(0)))
  private val aSubagentItem = SubagentItem(aSubagentId, aAgentRef.path, Uri("http://a.example.com"),
    itemRevision = Some(ItemRevision(0)))
  private val bSubagentItem = SubagentItem(bSubagentId, bAgentRef.path, Uri("http://b.example.com"),
    itemRevision = Some(ItemRevision(0)))
  private val lock = Lock(LockPath("LOCK"), itemRevision = Some(ItemRevision(0)))

  private val aJobResource = JobResource(
    JobResourcePath("A-JOB-RESOURCE"),
    itemRevision = Some(ItemRevision(0)))

  // Referenced only by Workflow.orderVariables
  private val bJobResource = JobResource(
    JobResourcePath("B-JOB-RESOURCE"),
    itemRevision = Some(ItemRevision(0)),
    variables = Map(
      "VARIABLE" -> StringConstant("VARIABLE-VALUE")))

  private val v1 = VersionId("1")
  private val v2 = VersionId("2")
  private val v3 = VersionId("3")
  private val v4 = VersionId("4")
  private val v5 = VersionId("5")

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ v1, Seq(execute(aAgentRef.path)))

  private val requiredParameter = OrderParameter.Required("required", NumberValue)

  private val bWorkflow = Workflow(
    WorkflowPath("B-WORKFLOW") ~ v1,
    Seq(
      LockInstruction.single(
        lock.path,
        None,
        Workflow.of(execute(bAgentRef.path)))),
    orderPreparation = OrderPreparation(OrderParameterList(View(
      requiredParameter,
      OrderParameter("hasDefault", StringConstant("DEFAULT")),
      // Constants are not stored in Order but re-evaluated with each access
      OrderParameter.Final("final", expr"'CONSTANT'"),
      // Other expressions are evaluated once and the results are stored as order arguments
      OrderParameter.Final("variable", expr"JobResource:B-JOB-RESOURCE:VARIABLE")))),
    jobResourcePaths = Seq(aJobResource.path))

  private val aOrderId = OrderId("A-ORDER")
  private val bOrderId = OrderId("B-ORDER")

  private def execute(agentPath: AgentPath) =
    Execute(
      WorkflowJob(
        agentPath, InternalExecutable("UNKNOWN")))


  private class Executor(var coll: EventColl[ControllerState, Event, TimeCtx])
    (using IORuntime):
    def controllerState = coll.aggregate

    def this(controllerState: ControllerState = ControllerState.empty)(using IORuntime) =
      this(EventColl(controllerState, timeCtx))

    def executeVerifiedUpdateItems(verifiedUpdateItems: VerifiedUpdateItems)
    : Checked[Seq[AnyKeyedEvent]] =
      for
        coll <- coll.addChecked:
          VerifiedUpdateItemsExecutor.execute(verifiedUpdateItems, controllerState)
        coll <- ControllerStateExecutor.addSubsequentEvents(coll)
      yield
        update(coll)
        coll.keyedEvents

    def execute[E <: Event, Ctx >: TimeCtx](eventCalc: EventCalc[ControllerState, E, Ctx])
    : Checked[Seq[KeyedEvent[Event]]] =
      for
        coll <- coll.add(eventCalc)
        subsequentColl <- ControllerStateExecutor.directSubsequentEvents(coll)
        coll <- coll.add(subsequentColl)
      yield
        update(coll)
        subsequentColl.keyedEvents

    def applyEvents(keyedEvents: KeyedEvent[Event]*): Unit =
      coll.add(keyedEvents)
        .map: coll =>
          update(coll)
          coll
        .orThrow

    /** @return subsequent events. */
    def applyWithSubsequentEvents(keyedEvents: Seq[KeyedEvent[Event]])
    : Checked[Seq[AnyKeyedEvent]] =
      for
        coll <- coll.add(keyedEvents)
        subsequentEvents <- ControllerStateExecutor.directSubsequentEvents(coll)
        coll <- coll.add(subsequentEvents)
      yield
        update(coll)
        subsequentEvents.keyedEvents

    private def update(coll: EventColl[ControllerState, Event, TimeCtx]): Unit =
      assert(coll.originalAggregate == this.coll.aggregate) // fails better then eq
      assert(coll.originalAggregate eq this.coll.aggregate)
      assert(coll.aggregate.toRecovered == coll.aggregate)
      this.coll = coll.forward

    def toSnapshot: Seq[Any] =
      controllerState.toSnapshotStream.compile.toVector
