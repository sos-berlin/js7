package js7.data.controller

import js7.base.crypt.silly.SillySigner
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerStateExecutor.convertImplicitly
import js7.data.controller.ControllerStateExecutorTest._
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemDestroyed, ItemDetachable, ItemDetached}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemDeleted}
import js7.data.item.{ItemRevision, ItemSigner, VersionId}
import js7.data.job.{InternalExecutable, JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderLockAcquired, OrderMoved, OrderRemoved, OrderStarted}
import js7.data.order.OrderId
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, LockInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

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
        Verified(itemSigner.sign(jobResource.withRevision(None)), Nil)),
      delete = Nil),
    Some(VerifiedUpdateItems.Versioned(
      v1,
      Seq(
        Verified(itemSigner.sign(aWorkflow), Nil),
        Verified(itemSigner.sign(bWorkflow), Nil)),
      delete = Nil)))

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
            delete = Nil))
        )) == Left(Problem.Combined(Set(
          MissingReferencedItemProblem(aWorkflow.id, aAgentRef.path),
          MissingReferencedItemProblem(bWorkflow.id, bAgentRef.path),
          MissingReferencedItemProblem(bWorkflow.id, lock.path),
          MissingReferencedItemProblem(bWorkflow.id, jobResource.path)))))
    }

    "Delete AgentRef but it is in use" in {
      val executor = new Executor(ControllerState.empty)

      executor.executeVerifiedUpdateItems(verifiedUpdateItems)
        .orThrow

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Nil, Nil,
            delete = Seq(aAgentRef.path)),
          None
        )) == Left(ItemIsStillReferencedProblem(aAgentRef.path, aWorkflow.id)))

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Nil, Nil,
            delete = Seq(aAgentRef.path)),
          Some(VerifiedUpdateItems.Versioned(v2, Nil,
            delete = Seq(aWorkflow.path)))
        )) == Right(Seq(
          NoKey <-: ItemDestroyed(aAgentRef.path),
          NoKey <-: VersionAdded(v2),
          NoKey <-: VersionedItemDeleted(aWorkflow.path),
          NoKey <-: ItemDestroyed(aWorkflow.id))))
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
        ControllerState.empty.executeVerifiedUpdateItems(verifiedUpdateItems) == Right(Seq[AnyKeyedEvent](
          NoKey <-: SignedItemAdded(itemSigner.sign(jobResource)),
          NoKey <-: UnsignedSimpleItemAdded(aAgentRef),
          NoKey <-: UnsignedSimpleItemAdded(bAgentRef),
          NoKey <-: UnsignedSimpleItemAdded(lock),
          NoKey <-: VersionAdded(v1),
          NoKey <-: VersionedItemAdded(itemSigner.sign(aWorkflow)),
          NoKey <-: VersionedItemAdded(itemSigner.sign(bWorkflow)))))

      _controllerState = updated // FIXME
    }

    //"After VersionedItemDeleted, the unused workflows are destroyed" in {
    //  val executor = new Executor(ControllerState.empty)
    //
    //  executor.executeVerifiedUpdateItems(verifiedUpdateItems)
    //    .orThrow
    //
    //  assert(
    //    executor.applyEventsAndReturnSubsequentEvents(Seq(
    //      VersionAdded(v2),
    //      VersionedItemDeleted(aWorkflow.path),
    //      VersionedItemDeleted(bWorkflow.path))
    //    ).map(_.toSet) == Right(Set[AnyKeyedEvent](
    //      NoKey <-: ItemDestroyed(aWorkflow.id),
    //      NoKey <-: ItemDestroyed(bWorkflow.id))))
    //}

    "Workflow is destroyed after last OrderRemoved" in {
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
          VersionedItemDeleted(aWorkflow.path)
        )) == Right(Nil))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          NoKey <-: ItemAttached(jobResource.path, Some(ItemRevision(0)), bAgentRef.path),
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
          aOrderId <-: OrderRemoved,
          bOrderId <-: OrderRemoved
        )) == Right(Seq(
          NoKey <-: ItemDetachable(aWorkflow.id, aAgentRef.path))))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          NoKey <-: ItemDetached(aWorkflow.id, aAgentRef.path)
        )) == Right(Seq(
          NoKey <-: ItemDestroyed(aWorkflow.id))))

      assert(
        executor.executeVerifiedUpdateItems(VerifiedUpdateItems(
          VerifiedUpdateItems.Simple(Nil, Nil, Nil),
          Some(VerifiedUpdateItems.Versioned(
            v3,
            Nil,
            delete = Seq(bWorkflow.path)))
        )) == Right(Seq(
          NoKey <-: VersionAdded(v3),
          NoKey <-: VersionedItemDeleted(bWorkflow.path),
          NoKey <-: ItemDetachable(bWorkflow.id, bAgentRef.path))))

      assert(
        executor.applyEventsAndReturnSubsequentEvents(Seq(
          NoKey <-: ItemDetached(bWorkflow.id, bAgentRef.path)
        )) == Right(Seq(
          NoKey <-: ItemDestroyed(bWorkflow.id))))
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
        aOrderId <-: OrderRemovalMarked)
      == Right(Seq(
        aOrderId <-: OrderRemoved)))

    assert(applyEvents(_controllerState,
      VersionAdded(v2),
      VersionedItemDeleted(aWorkflow.path),
      VersionedItemDeleted(bWorkflow.path),
      VersionedItemDeleted(cWorkflow.path)
    ) == Right(Seq(
      NoKey <-: ItemDestroyed(cWorkflow.id),
      NoKey <-: ItemDetachable(aWorkflow.id, aAgentRef.path))))

    assert(applyEvents(_controllerState,
      ItemDestructionMarked(aAgentRef.path)
    ) == Right(Nil))

    assert(applyEvents(_controllerState,
      ItemDestructionMarked(bAgentRef.path))
      == Right(Seq(
        NoKey <-: ItemDetachable(bWorkflow.id, bAgentRef.path))))

    assert(applyEvents(_controllerState,
      ItemDetached(aWorkflow.id, aAgentRef.path)
    ) == Right(Seq(
      NoKey <-: ItemDestroyed(aAgentRef.path))))
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
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"), itemRevision = Some(ItemRevision(0)))
  private val v1 = VersionId("1")
  private val v2 = VersionId("2")
  private val v3 = VersionId("3")
  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ v1, Seq(execute(aAgentRef.path)))
  private val bWorkflow = Workflow(
    WorkflowPath("B-WORKFLOW") ~ v1,
    Seq(
      LockInstruction(
        lock.path,
        None,
        Workflow.of(execute(bAgentRef.path)))),
    jobResourcePaths = Seq(jobResource.path))
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
      controllerState.executeVerifiedUpdateItems(verifiedUpdateItems)
        .flatMap(keyedEvents =>
          applyEventsAndReturnSubsequentEvents(keyedEvents)
            .map(keyedEvents ++ _))

    def applyEventsAndReturnSubsequentEvents(keyedEvents: Seq[KeyedEvent[Event]])
    : Checked[Seq[KeyedEvent[Event]]] =
      for (eventsAndState <- controllerState.applyEventsAndReturnSubsequentEvents(keyedEvents)) yield {
        controllerState = eventsAndState.controllerState
        eventsAndState.keyedEvents.toVector
      }
  }
}
