package js7.data.controller

import js7.base.crypt.silly.SillySigner
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem, UnknownItemPathProblem}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerStateExecutor.convertImplicitly
import js7.data.controller.ControllerStateExecutorTest._
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemRemoved}
import js7.data.item.{ItemRevision, ItemSigner, VersionId}
import js7.data.job.{InternalExecutable, JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderLockAcquired, OrderMoved, OrderStarted}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.OrderParameters.{MissingOrderArgumentProblem, WrongOrderArgumentTypeProblem}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, LockInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{OrderParameter, OrderParameters, OrderPreparation, Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View

final class ControllerStateExecutorTest extends AnyFreeSpec
{
  "resetAgent" in {
    pending // TODO
  }

  private lazy val verifiedUpdateItems = VerifiedUpdateItems(
    VerifiedUpdateItems.Simple(
      Seq(
        aAgentRef.withRevision(None),
        bAgentRef.withRevision(None),
        lock.withRevision(None)),
      Seq(
        Verified(itemSigner.sign(aJobResource.withRevision(None)), Nil),
        Verified(itemSigner.sign(bJobResource.withRevision(None)), Nil)),
      delete = Nil),
    Some(VerifiedUpdateItems.Versioned(
      v1,
      Seq(
        Verified(itemSigner.sign(aWorkflow), Nil),
        Verified(itemSigner.sign(bWorkflow), Nil)),
      remove = Nil)))

  "executeVerifiedUpdateItems" - {
    "Add workflow but referenced items are missing" in {
      val executor = new Executor(ControllerState.empty)
      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Nil, Nil, Nil),
          Some(VerifiedUpdateItems.Versioned(
            v1,
            Seq(
              Verified(itemSigner.sign(aWorkflow), Nil),
              Verified(itemSigner.sign(bWorkflow), Nil)),
            remove = Nil))
        )) == Left(Problem.Combined(Set(
          MissingReferencedItemProblem(aWorkflow.id, aAgentRef.path),
          MissingReferencedItemProblem(bWorkflow.id, bAgentRef.path),
          MissingReferencedItemProblem(bWorkflow.id, lock.path),
          MissingReferencedItemProblem(bWorkflow.id, aJobResource.path),
          MissingReferencedItemProblem(bWorkflow.id, bJobResource.path)))))
    }

    "Delete AgentRef but it is in use by a workflow" in {
      val executor = new Executor(ControllerState.empty)

      executor.executeVerifiedUpdateItems(verifiedUpdateItems)
        .orThrow

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aAgentRef.path))
        )) == Left(ItemIsStillReferencedProblem(aAgentRef.path, aWorkflow.id)))

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aAgentRef.path)),
          Some(VerifiedUpdateItems.Versioned(v2, remove = Seq(aWorkflow.path)))
        )) == Right(Seq(
          NoKey <-: ItemDeleted(aAgentRef.path),
          NoKey <-: VersionAdded(v2),
          NoKey <-: VersionedItemRemoved(aWorkflow.path),
          NoKey <-: ItemDeleted(aWorkflow.id))))
    }

    "Delete AgentRef but it is in use by a deleted workflow still containing orders" in {
      val executor = new Executor(ControllerState.empty)

      executor.executeVerifiedUpdateItems(verifiedUpdateItems)
        .orThrow

      val orderId = OrderId("ORDER")
      executor.controllerState = executor.controllerState
        .applyEvent(orderId <-: OrderAdded(aWorkflow.id))
        .orThrow

      executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
        VerifiedUpdateItems.Simple(),
        Some(VerifiedUpdateItems.Versioned(VersionId("2"), remove = Seq(aWorkflow.path)))
      )).orThrow

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aAgentRef.path))
        )) == Left(ItemIsStillReferencedProblem(aAgentRef.path, aWorkflow.id)))

      executor
        .applyEventsAndReturnSubsequentEvents(Seq(
          orderId <-: OrderCancelled,
          orderId <-: OrderDeleted))
        .orThrow

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(aAgentRef.path))
        )) == Right(Seq(
          NoKey <-: ItemDeleted(aAgentRef.path))))
    }

    // TODO Don't use ControllerStateTest here
    import ControllerStateTest.{controllerState, fileWatch, workflow}

    "Empty" in {
      val executor = new Executor(controllerState)
      executor
        .executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(),
          None))
        .orThrow
    }

    "Delete and add fileWatch" in {
      val executor = new Executor(controllerState)

      // Delete the fileWatch
      assert(executor.controllerState.keyToItem.contains(fileWatch.path))
      executor
        .executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(delete = Seq(fileWatch.path))))
        .orThrow
      assert(!executor.controllerState.keyToItem.contains(fileWatch.path))

      locally {
        // Delete the workflow
        val deletedWorkflow = new Executor(executor.controllerState)
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
      }

      // Add the workflow
      executor
        .executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Seq(fileWatch.withRevision(None)))))
        .orThrow

      assert(executor.controllerState.keyToItem.contains(fileWatch.path))
    }
  }

  "addOrders" - {
    val aOrderId = OrderId("A")
    val bOrderId = OrderId("B")
    val executor = new Executor(ControllerState.empty)

    "addOrder for unknown workflow is rejected" in {
      assert(executor.controllerState.addOrders(Seq(FreshOrder(aOrderId, aWorkflow.path))) ==
        Left(UnknownItemPathProblem(aWorkflow.path)))
    }

    "addOrder with required argument is rejected" in {
      executor.executeVerifiedUpdateItems(verifiedUpdateItems).orThrow
      assert(
        executor.execute(_.addOrders(Seq(
          FreshOrder(aOrderId, aWorkflow.path),
          FreshOrder(bOrderId, bWorkflow.path)
        ))) ==
          Left(MissingOrderArgumentProblem(requiredParameter)))
    }

    "addOrder with wrong argument type is rejected" in {
      assert(
        executor.execute(_.addOrders(Seq(
          FreshOrder(bOrderId, bWorkflow.path, Map("required" -> StringValue("STRING")))
        ))) == Left(WrongOrderArgumentTypeProblem(requiredParameter, StringValue)))
    }

    "addOrder resolved default argument and provides variables" in {
      executor.execute(_.addOrders(Seq(
        FreshOrder(aOrderId, aWorkflow.path),
        FreshOrder(bOrderId, bWorkflow.path, Map("required" -> NumberValue(7))))))

      assert(executor.controllerState.idToOrder.values.toSet == Set(
        Order(aOrderId, aWorkflow.id /: Position(0), Order.Fresh,
          attachedState = Some(Order.Attaching(aAgentRef.path))),
        Order(bOrderId, bWorkflow.id /: (Position(0) / "lock" % 0), Order.Ready,
          arguments = Map(
            "required" -> NumberValue(7),
            "variable" -> StringValue("VARIABLE-VALUE")),
          attachedState = Some(Order.Attaching(bAgentRef.path)))))
    }
  }

  "applyEventsAndReturnSubsequentEvents" - {
    var _controllerState = ControllerState.empty
    var updated = ControllerState.empty

    def applyEvents(keyedEvents: KeyedEvent[Event]*) = {
      val executor = new Executor(_controllerState)
      val result = executor.applyEventsAndReturnSubsequentEvents(keyedEvents)
      updated = executor.controllerState
      result
    }

    "No events" in {
      assert(applyEvents() == Right(Nil))
    }

    "Add agents, workflows and orders" in {
      assert(
        VerifiedUpdateItemsExecutor.execute(verifiedUpdateItems, ControllerState.empty) ==
          Right(Seq[AnyKeyedEvent](
            NoKey <-: SignedItemAdded(itemSigner.sign(aJobResource)),
            NoKey <-: SignedItemAdded(itemSigner.sign(bJobResource)),
            NoKey <-: UnsignedSimpleItemAdded(aAgentRef),
            NoKey <-: UnsignedSimpleItemAdded(bAgentRef),
            NoKey <-: UnsignedSimpleItemAdded(lock),
            NoKey <-: VersionAdded(v1),
            NoKey <-: VersionedItemAdded(itemSigner.sign(aWorkflow)),
            NoKey <-: VersionedItemAdded(itemSigner.sign(bWorkflow)))))

      _controllerState = updated
    }

    //"After VersionedItemRemoved, the unused workflows are deleted" in {
    //  val executor = new Executor(ControllerState.empty)
    //
    //  executor.executeVerifiedUpdateItems(verifiedUpdateItems)
    //    .orThrow
    //
    //  assert(
    //    executor.applyEventsAndReturnSubsequentEvents(Seq(
    //      VersionAdded(v2),
    //      VersionedItemRemoved(aWorkflow.path),
    //      VersionedItemRemoved(bWorkflow.path))
    //    ).map(_.toSet) == Right(Set[AnyKeyedEvent](
    //      NoKey <-: ItemDeleted(aWorkflow.id),
    //      NoKey <-: ItemDeleted(bWorkflow.id))))
    //}

    "Workflow is deleted after last OrderDeleted" in {
      val executor = new Executor(ControllerState.empty)

      executor.executeVerifiedUpdateItems(verifiedUpdateItems)
        .orThrow

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          aOrderId <-: OrderAdded(aWorkflow.id),
          bOrderId <-: OrderAdded(bWorkflow.id)
        )) == Right(Seq(
          aOrderId <-: OrderAttachable(aAgentRef.path),
          bOrderId <-: OrderStarted,
          bOrderId <-: OrderLockAcquired(lock.path),
          bOrderId <-: OrderAttachable(bAgentRef.path))))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          VersionAdded(v2),
          VersionedItemRemoved(aWorkflow.path)
        )) == Right(Nil))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          NoKey <-: ItemAttached(aJobResource.path, Some(ItemRevision(0)), bAgentRef.path),
          NoKey <-: ItemAttached(aWorkflow.id, None, aAgentRef.path),
          NoKey <-: ItemAttached(bWorkflow.id, None, bAgentRef.path),
          aOrderId <-: OrderAttached(aAgentRef.path),
          bOrderId <-: OrderAttached(bAgentRef.path),
          aOrderId <-: OrderMoved(Position(1)),
          bOrderId <-: OrderMoved(Position(1))
        )) == Right(Nil))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          aOrderId <-: OrderDetachable,
          bOrderId <-: OrderDetachable
        )) == Right(Nil))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          aOrderId <-: OrderDetached,
          bOrderId <-: OrderDetached
        )) == Right(Seq(
          aOrderId <-: OrderStarted,
          aOrderId <-: OrderFinished,
          bOrderId <-: OrderFinished)))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          aOrderId <-: OrderDeleted,
          bOrderId <-: OrderDeleted
        )) == Right(Seq(
          NoKey <-: ItemDetachable(aWorkflow.id, aAgentRef.path))))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
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
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          NoKey <-: ItemDetached(bWorkflow.id, bAgentRef.path)
        )) == Right(Seq(
          NoKey <-: ItemDeleted(bWorkflow.id))))
    }

/*
    assert(
      applyEvents(_controllerState,
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
      applyEvents(_controllerState,
        aOrderId <-: OrderDetachable,
        aOrderId <-: OrderDetached,
        bOrderId <-: OrderDetachable
      ) == Right(Seq(
        aOrderId <-: OrderStarted,
        aOrderId <-: OrderFinished)))

    assert(
      applyEvents(_controllerState,
        aOrderId <-: OrderDeletionMarked)
      == Right(Seq(
        aOrderId <-: OrderDeleted)))

    assert(applyEvents(_controllerState,
      VersionAdded(v2),
      VersionedItemRemoved(aWorkflow.path),
      VersionedItemRemoved(bWorkflow.path),
      VersionedItemRemoved(cWorkflow.path)
    ) == Right(Seq(
      NoKey <-: ItemDeleted(cWorkflow.id),
      NoKey <-: ItemDetachable(aWorkflow.id, aAgentRef.path))))

    assert(applyEvents(_controllerState,
      ItemDeletionMarked(aAgentRef.path)
    ) == Right(Nil))

    assert(applyEvents(_controllerState,
      ItemDeletionMarked(bAgentRef.path))
      == Right(Seq(
        NoKey <-: ItemDetachable(bWorkflow.id, bAgentRef.path))))

    assert(applyEvents(_controllerState,
      ItemDetached(aWorkflow.id, aAgentRef.path)
    ) == Right(Seq(
      NoKey <-: ItemDeleted(aAgentRef.path))))
 */
  }

  "nextOrderWatchOrderEvents" in {
    pending // TODO
  }

  "nextOrderEventsByOrderId" in {
    pending // TODO
  }

  "TEST WIP" in {
    final case class X(controllerState: ControllerState, keyedEvents: Seq[AnyKeyedEvent] = Nil) {
      def doSomething: Checked[X] = {
        val keyedEvents = Seq(NoKey <-: VersionAdded(VersionId("TEST")))
        controllerState.applyEvents(keyedEvents)
          .map(X(_, keyedEvents))
      }
    }
    val controllerState = ControllerState.empty
    for {
      x <- X(controllerState).doSomething
      x <- x.doSomething
    } yield x
  }
}

object ControllerStateExecutorTest
{
  private val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)

  private val aAgentRef = AgentRef(AgentPath("A-AGENT"), Uri("http://0.0.0.0:0"), Some(ItemRevision(0)))
  private val bAgentRef = AgentRef(AgentPath("B-AGENT"), Uri("http://0.0.0.0:0"), Some(ItemRevision(0)))
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

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ v1, Seq(execute(aAgentRef.path)))

  private val requiredParameter = OrderParameter.Required("required", NumberValue)

  private val bWorkflow = Workflow(
    WorkflowPath("B-WORKFLOW") ~ v1,
    Seq(
      LockInstruction(
        lock.path,
        None,
        Workflow.of(execute(bAgentRef.path)))),
    orderPreparation = OrderPreparation(OrderParameters(View(
      requiredParameter,
      OrderParameter("hasDefault", StringConstant("DEFAULT")),
      // Constants are not stored in Order but re-evaluated with each access
      OrderParameter.Final("final", expr("'CONSTANT'")),
      // Other expressions are evaluated once and the results are stored as order arguments
      OrderParameter.Final("variable", expr("JobResource:B-JOB-RESOURCE:VARIABLE"))))),
    jobResourcePaths = Seq(aJobResource.path))

  private val aOrderId = OrderId("A-ORDER")
  private val bOrderId = OrderId("B-ORDER")

  private def execute(agentPath: AgentPath) =
    Execute(
      WorkflowJob(
        agentPath, InternalExecutable("UNKNOWN")))

  private class Executor(var controllerState: ControllerState)
  {
    def executeVerifiedUpdateItems(verifiedUpdateItems: VerifiedUpdateItems)
    : Checked[Seq[AnyKeyedEvent]] =
      VerifiedUpdateItemsExecutor.execute(verifiedUpdateItems, controllerState)
        .flatMap(keyedEvents =>
          applyEventsAndReturnSubsequentEvents(keyedEvents)
            .map(keyedEvents ++ _))

    def execute(body: ControllerState => Checked[Seq[KeyedEvent[Event]]])
    : Checked[Seq[KeyedEvent[Event]]] =
      for {
        events <- body(controllerState)
        events <- applyEventsAndReturnSubsequentEvents(events)
      } yield events

    def applyEventsAndReturnSubsequentEvents(keyedEvents: Seq[KeyedEvent[Event]])
    : Checked[Seq[KeyedEvent[Event]]] =
      for (eventsAndState <- controllerState.applyEventsAndReturnSubsequentEvents(keyedEvents)) yield {
        controllerState = eventsAndState.controllerState
        eventsAndState.keyedEvents.toVector
      }

  }
}
