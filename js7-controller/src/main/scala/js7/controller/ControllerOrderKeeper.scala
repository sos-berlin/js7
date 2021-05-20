package js7.controller

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import cats.instances.either._
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.traverse._
import java.time.ZoneId
import js7.agent.data.event.AgentControllerEvent
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.ops._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.monixutils.MonixDeadline.syntax._
import js7.base.problem.Checked._
import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Collections.implicits._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.cluster.WorkingClusterNode
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.common.akkautils.SupervisorStrategies
import js7.controller.ControllerOrderKeeper._
import js7.controller.agent.{AgentDriver, AgentDriverConfiguration}
import js7.controller.configuration.ControllerConfiguration
import js7.controller.item.VerifiedUpdateItems
import js7.controller.problems.ControllerIsNotYetReadyProblem
import js7.core.command.CommandMeta
import js7.core.common.ActorRegister
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.{CannotRemoveOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentRefStateEvent.{AgentEventsObserved, AgentReady}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerStateExecutor.{liveOrderEventHandler, liveOrderEventSource}
import js7.data.controller.{ControllerCommand, ControllerEvent, ControllerState, ControllerStateExecutor}
import js7.data.crypt.SignedItemVerifier
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, JournalHeader, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedToAgent, ItemDeletionMarked, ItemDestroyed, ItemDetached}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, Detached}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{SimpleItemAdded, SimpleItemChanged, UnsignedSimpleItemAddedOrChanged}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemEvent}
import js7.data.item.{InventoryItemEvent, InventoryItemKey, ItemRevision, SignableSimpleItem, SignableSimpleItemPath, UnsignedSimpleItem, VersionedEvent}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderAttachable, OrderAttached, OrderCancelMarked, OrderCancelMarkedOnAgent, OrderCoreEvent, OrderDetachable, OrderDetached, OrderRemoveMarked, OrderRemoved, OrderResumeMarked, OrderSuspendMarked, OrderSuspendMarkedOnAgent}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.{OrderWatchEvent, OrderWatchPath}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow}
import js7.journal.recover.Recovered
import js7.journal.state.JournaledStatePersistence
import js7.journal.{JournalActor, MainJournalingActor}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.SerialCancelable
import scala.collection.immutable.VectorBuilder
import scala.collection.{View, mutable}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class ControllerOrderKeeper(
  stopped: Promise[ControllerTermination],
  persistence: JournaledStatePersistence[ControllerState],
  clusterNode: WorkingClusterNode[ControllerState],
  controllerConfiguration: ControllerConfiguration,
  testEventPublisher: EventPublisher[Any])
  (implicit protected val scheduler: Scheduler)
extends Stash
with MainJournalingActor[ControllerState, Event]
{
  import context.{actorOf, watch}
  import controllerConfiguration.config
  import js7.controller.ControllerOrderKeeper.RichIdToOrder

  override val supervisorStrategy = SupervisorStrategies.escalate
  protected def journalConf = controllerConfiguration.journalConf
  protected def journalActor = persistence.journalActor

  private val agentDriverConfiguration = AgentDriverConfiguration.fromConfig(config, controllerConfiguration.journalConf).orThrow
  private var _controllerState: ControllerState = ControllerState.Undefined
  private val orderEventSource = liveOrderEventSource(() => _controllerState)
  private val orderEventHandler = liveOrderEventHandler(() => _controllerState)

  private val agentRegister = new AgentRegister
  private val orderRegister = mutable.HashMap.empty[OrderId, OrderEntry]
  private val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  private val removeOrderDelay = config.getDuration("js7.order.remove-delay").toFiniteDuration
  private val testAddOrderDelay = config.optionAs[FiniteDuration]("js7.TEST-ONLY.add-order-delay").fold(Task.unit)(Task.sleep)
  private var journalTerminated = false

  private object shutdown {
    var delayUntil = now
    val since = SetOnce[MonixDeadline]
    private val shutDown = SetOnce[ControllerCommand.ShutDown]
    private val stillShuttingDownCancelable = SerialCancelable()
    private var terminatingAgentDrivers = false
    private var takingSnapshot = false
    private var snapshotTaken = false
    private var terminatingJournal = false

    def shuttingDown = since.isDefined

    def restart = shutDown.toOption.fold(false)(_.restart)

    def start(shutDown: ControllerCommand.ShutDown): Unit =
      if (!shuttingDown) {
        since := now
        this.shutDown := shutDown
        stillShuttingDownCancelable := scheduler.scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations/*?*/) {
          self ! Internal.StillShuttingDown
        }
        continue()
      }

    def close() =
      stillShuttingDownCancelable.cancel()

    def onStillShuttingDown() =
      logger.info(s"Still shutting down, waiting for ${agentRegister.runningActorCount} AgentDrivers" +
        (!snapshotTaken ?? " and the snapshot"))

    def onSnapshotTaken(): Unit =
      if (shuttingDown) {
        snapshotTaken = true
        continue()
      }

    def continue() =
      for (shutDown <- shutDown) {
        logger.trace(s"shutdown.continue: ${agentRegister.runningActorCount} AgentDrivers${!snapshotTaken ?? ", snapshot required"}")
        if (!terminatingAgentDrivers) {
          terminatingAgentDrivers = true
          agentRegister.values foreach {
            _.actor ! AgentDriver.Input.Terminate()
          }
        }
        if (agentRegister.runningActorCount == 0) {
          if (!takingSnapshot) {
            takingSnapshot = true
            if (shutDown.suppressSnapshot) {
              snapshotTaken = true
            } else {
              journalActor ! JournalActor.Input.TakeSnapshot
            }
          }
          if (snapshotTaken && !terminatingJournal) {
            // The event forces the cluster to acknowledge this event and the snapshot taken
            terminatingJournal = true
            persistKeyedEventTask(NoKey <-: ControllerShutDown())((_, _) => Completed)
              .tapEval(_ => persistence.stop)
              .runToFuture
              .onComplete {
                case Success(Right(Completed)) =>
                case other => logger.error(s"While shutting down: $other")
              }
          }
        }
      }
  }
  import shutdown.shuttingDown

  /** Next orders to be processed. */
  private object orderQueue {
    private val queue = new VectorBuilder[OrderId]
    private var notified = false

    def enqueue(orderIds: Iterable[OrderId]): Unit =
      if (!shuttingDown && switchover.isEmpty && orderIds.nonEmpty) {
        queue ++= orderIds
        notifiy()
      }

    def readAll(): Seq[OrderId] = {
      notified = false
      val orderIds = queue.result().distinct
      queue.clear()
      orderIds
    }

    private def notifiy(): Unit =
      if (!notified) {
        self ! Internal.ContinueWithNextOrderEvents
        notified = true
      }

    override def toString = queue.result().map(_.string).mkString(", ")  // For debugging
  }

  @volatile
  private var switchover: Option[Switchover] = None

  private final class Switchover(val restart: Boolean) {
    // 1) Emit SwitchedOver event
    // 2) Terminate JournalActor
    // 3) Stop ControllerOrderKeeper includinge AgentDriver's
    // Do not terminate AgentDrivers properly because we do not want any events.

    private val stillSwitchingOverSchedule = scheduler.scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations) {
      logger.debug("Still switching over to the other cluster node")
    }

    def start(): Task[Checked[Completed]] =
      clusterNode.switchOver   // Will terminate `cluster`, letting ControllerOrderKeeper terminate
        .flatMapT(o => persistence.stop.as(Right(o)))

    def close() = stillSwitchingOverSchedule.cancel()
  }

  watch(journalActor)

  override def postStop() =
    try {
      clusterNode.close()
      shutdown.close()
      switchover foreach { _.close() }
    } finally {
      logger.debug("Stopped" + shutdown.since.toOption.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))
      stopped.success(
        if (switchover.exists(_.restart)) ControllerTermination.Restart
        else ControllerTermination.Terminate(restart = shutdown.restart))
      super.postStop()
    }

  def receive = {
    case Input.Start(recovered) =>
      assertActiveClusterState(recovered)
      recover(recovered)

      become("activating")(activating)
      unstashAll()
      clusterNode.beforeJournalingStarts
        .map(_.orThrow)
        .map((_: Completed) => ())
        .materialize
        .map(Internal.Activated.apply)
        .runToFuture
        .pipeTo(self)

    case msg => notYetReady(msg)
  }

  private def assertActiveClusterState(recovered: Recovered[ControllerState]): Unit =
    for (clusterState <- recovered.recoveredState.map(_.clusterState)) {
      import controllerConfiguration.clusterConf.ownId
      if (!clusterState.isEmptyOrActive(ownId))
        throw new IllegalStateException(
          s"Controller has recovered from Journal but is not the active node in ClusterState: " +
            s"id=$ownId, failedOver=$clusterState")
    }

  private def recover(recovered: Recovered[ControllerState]): Unit = {
    for (controllerState <- recovered.recoveredState) {
      if (controllerState.controllerMetaState.controllerId != controllerConfiguration.controllerId)
        throw Problem(s"Recovered controllerId='${controllerState.controllerMetaState.controllerId}' " +
          s"differs from configured controllerId='${controllerConfiguration.controllerId}'"
        ).throwable
      this._controllerState = controllerState
      //controllerMetaState = controllerState.controllerMetaState.copy(totalRunningTime = recovered.totalRunningTime)
      for (agentRef <- controllerState.pathToAgentRefState.values.map(_.agentRef)) {
        val agentRefState = controllerState.pathToAgentRefState.getOrElse(agentRef.path, AgentRefState(agentRef))
        registerAgent(agentRef, agentRefState.agentRunId, eventId = agentRefState.eventId)
      }

      persistedEventId = controllerState.eventId
    }
  }

  private def activating: Receive = {
    case Internal.Activated(Failure(t)) =>
      logger.error(s"Activation of this cluster node failed because: ${t.toStringWithCauses}")
      if (t.getStackTrace.nonEmpty) logger.debug(t.toStringWithCauses, t)
      throw t.appendCurrentStackTrace

    case Internal.Activated(Success(())) =>
      val sender = this.sender()
      persistence.journalHeader
        .runToFuture
        .onComplete { tried => (self ! Internal.JournalIsReady(tried))(sender) }
      become("journalIsStarting")(journalIsStarting)
      unstashAll()

    case msg => notYetReady(msg)
  }

  private def notYetReady(message: Any): Unit =
    message match {
      case Command.Execute(_: ControllerCommand.ShutDown, _) =>
        stash()

      case Command.Execute(cmd, _) =>
        logger.warn(s"$ControllerIsNotYetReadyProblem: $cmd")
        sender() ! Left(ControllerIsNotYetReadyProblem)

      case Command.VerifiedUpdateItemsCmd(_) =>
        logger.warn(s"$ControllerIsNotYetReadyProblem: VerifiedUpdateItemsCmd")
        sender() ! Left(ControllerIsNotYetReadyProblem)

      case cmd: Command =>
        logger.warn(s"$ControllerIsNotYetReadyProblem: $cmd")
        sender() ! Status.Failure(ControllerIsNotYetReadyProblem.throwable)

      case _ => stash()
    }

  private def journalIsStarting: Receive = {
    case Internal.JournalIsReady(Failure(t)) =>
      logger.error(t.toStringWithCauses)
      throw t

    case Internal.JournalIsReady(Success(journalHeader)) =>
      become("becomingReady")(becomingReady)  // `become` must be called early, before any persist!

      locally {
        val maybeControllerInitialized = !_controllerState.controllerMetaState.isDefined thenVector
          (NoKey <-: ControllerEvent.ControllerInitialized(
            controllerConfiguration.controllerId,
            journalHeader.startedAt))
        val controllerReady = NoKey <-: ControllerEvent.ControllerReady(
          ZoneId.systemDefault.getId,
          totalRunningTime = journalHeader.totalRunningTime)

        val events = maybeControllerInitialized :+ controllerReady :++
          new ControllerStateExecutor(_controllerState).nextOrderEvents

        persistMultiple(events) { (_, updatedState) =>
          _controllerState = updatedState
          clusterNode.afterJournalingStarted
            .materializeIntoChecked
            .runToFuture
            .map(Internal.Ready.apply)
            .pipeTo(self)
        }
      }

      _controllerState.allOrderWatchesState.pathToOrderWatchState.keys foreach proceedWithItem

      // Proceed order before starting AgentDrivers, so AgentDrivers may match recovered OrderIds with Agent's OrderIds
      orderRegister ++= _controllerState.idToOrder.keys.map(_ -> new OrderEntry(scheduler.now))

      if (persistedEventId > EventId.BeforeFirst) {  // Recovered?
        logger.info(s"${_controllerState.idToOrder.size} Orders, " +
          s"${_controllerState.repo.typedCount[Workflow]} Workflows and " +
          s"${_controllerState.pathToAgentRefState.size} AgentRefs recovered")
      }
      // Any ordering when continuing orders???
      proceedWithOrders(_controllerState.idToOrder.keys)
      orderQueue.enqueue(_controllerState.idToOrder.keys)

      // Start fetching events from Agents after AttachOrder has been sent to AgentDrivers.
      // This is to handle race-condition: An Agent may have already completed an order.
      // So send AttachOrder before DetachOrder.
      // The Agent will ignore the duplicate AttachOrder if it arrives before DetachOrder.
      agentRegister.values foreach {
        _.actor ! AgentDriver.Input.StartFetchingEvents
      }

    case Command.Execute(_: ControllerCommand.ShutDown, _) =>
      stash()

    case Command.Execute(cmd, _) =>
      logger.warn(s"$ControllerIsNotYetReadyProblem: $cmd")
      sender() ! Left(ControllerIsNotYetReadyProblem)

    case cmd: Command =>
      logger.warn(s"$ControllerIsNotYetReadyProblem: $cmd")
      sender() ! Status.Failure(ControllerIsNotYetReadyProblem.throwable)

    case _ => stash()
  }

  private def becomingReady: Receive = {
    case Internal.Ready(Left(problem)) =>
      logger.error(s"Appointment of configured cluster backup-node failed: $problem")
      throw problem.throwable.appendCurrentStackTrace

    case Internal.Ready(Right(Completed)) =>
      logger.info("Ready")
      testEventPublisher.publish(ControllerReadyTestIncident)
      clusterNode.onTerminatedUnexpectedly.runToFuture onComplete { tried =>
        self ! Internal.ClusterModuleTerminatedUnexpectedly(tried)
      }
      become("Ready")(ready orElse handleExceptionalMessage)
      unstashAll()

    case _ =>
      // stash Command too, after ControllerReady event and cluster node has been initialized (see above)
      stash()
  }

  private def ready: Receive = {
    case Internal.ContinueWithNextOrderEvents =>
      val orderIds = orderQueue.readAll()
      val keyedEvents = nextOrderEvents(orderIds)
      if (keyedEvents.nonEmpty) {
        persistTransaction(keyedEvents)(handleEvents)
      }

    case Command.Execute(command, meta) =>
      val sender = this.sender()
      if (shuttingDown)
        sender ! Status.Failure(ControllerIsShuttingDownProblem.throwable)
      else if (switchover.isDefined)
        sender ! Status.Failure(ControllerIsSwitchingOverProblem.throwable)
      else
        executeControllerCommand(command, meta) onComplete {
          case Failure(t) => sender ! Status.Failure(t)
          case Success(response) => sender ! response
        }

    case Command.VerifiedUpdateItemsCmd(verifiedUpdateItems: VerifiedUpdateItems) =>
      import verifiedUpdateItems.{maybeVersioned, simple}
      val t = now
      val checkedEvents: Checked[View[InventoryItemEvent]] = simpleItemsToEvents(simple)
      val whenPersisted = maybeVersioned
        .map(versionedItemsToEvent)
        .getOrElse(Success(Right(Nil)))
        match {
          case Failure(t) => Future.failed(t)
          case Success(Left(problem)) => Future.successful(Left(problem))
          case Success(Right(versionedEvents)) =>
            checkedEvents match {
              case Left(problem) => Future.successful(Left(problem))
              case Right(simpleItemEvents) =>
                val keyedEvents = simpleItemEvents.view.map(NoKey <-: _) ++ versionedEvents.map(NoKey <-: _)
                persistItemAndVersionedEvents(keyedEvents.toVector)
                  .map(_.map { o =>
                    if (t.elapsed > 1.s) logger.debug("VerifiedUpdateItemsCmd - " +
                      itemsPerSecondString(t.elapsed, verifiedUpdateItems.itemCount, "items"))
                    o
                  })
            }
        }
      val sender = this.sender()
      whenPersisted.onComplete {
        case Failure(t) => sender ! Status.Failure(t)
        case Success(response) => sender ! (response: Checked[Completed])
      }

    case AgentDriver.Output.EventsFromAgent(stampedAgentEvents, committedPromise) =>
      val agentEntry = agentRegister(sender())
      import agentEntry.agentPath
      var timestampedEvents: Seq[Timestamped[Event]] =
        stampedAgentEvents.view.flatMap {
          case Stamped(_, timestampMillis, keyedEvent) =>
            keyedEvent match {
              case KeyedEvent(orderId: OrderId, _: OrderCancelMarked) =>
                Timestamped(orderId <-: OrderCancelMarkedOnAgent, Some(timestampMillis)) :: Nil

              case KeyedEvent(orderId: OrderId, _: OrderSuspendMarked) =>
                Timestamped(orderId <-: OrderSuspendMarkedOnAgent, Some(timestampMillis)) :: Nil

              case KeyedEvent(_, _: OrderResumeMarked) =>
                Nil /*Agent does not emit OrderResumeMarked*/

              case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                val ownEvent = event match {
                  case _: OrderEvent.OrderAttachedToAgent => OrderAttached(agentPath) // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                  case _ => event
                }
                Timestamped(orderId <-: ownEvent, Some(timestampMillis)) :: Nil

              case KeyedEvent(_: NoKey, AgentControllerEvent.AgentReady(timezone, _)) =>
                Timestamped(agentEntry.agentPath <-: AgentReady(timezone), Some(timestampMillis)) :: Nil

              case KeyedEvent(_: NoKey, ItemAttachedToAgent(item)) =>
                // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                Timestamped(NoKey <-: ItemAttached(item.key, item.itemRevision, agentPath)) :: Nil

              case KeyedEvent(_: NoKey, _: ItemDetached) =>
                Timestamped(keyedEvent) :: Nil

              case KeyedEvent(_: OrderWatchPath, _: OrderWatchEvent) =>
                Timestamped(keyedEvent) :: Nil

              case _ =>
                logger.error(s"Unknown event received from ${agentEntry.agentPath}: $keyedEvent")
                Nil
            }
        }.toVector

      if (timestampedEvents.isEmpty) {
        // timestampedEvents may be empty if it contains only discarded (Agent-only) events.
        // Agent's last observed EventId is not persisted then, and we do not write an AgentEventsObserved.
        // For tests, this makes the journal predictable after OrderFinished (because no AgentEventsObserved may follow).
        committedPromise.success(None)
      } else {
        val agentEventId = stampedAgentEvents.last.eventId
        timestampedEvents :+= Timestamped(agentPath <-: AgentEventsObserved(agentEventId))

        val subseqEvents = subsequentEvents(timestampedEvents.map(_.keyedEvent))
        orderQueue.enqueue(subseqEvents.view.collect { case KeyedEvent(orderId: OrderId, _) => orderId })  // For OrderSourceEvents
        timestampedEvents ++= subseqEvents.map(Timestamped(_))

        committedPromise.completeWith(
          persistTransactionTimestamped(timestampedEvents, alreadyDelayed = agentDriverConfiguration.eventBufferDelay) {
            (stampedEvents, updatedState) =>
              handleEvents(stampedEvents, updatedState)
              Some(agentEventId)
          })
      }

    case AgentDriver.Output.OrdersMarked(orderToMark) =>
      val unknown = orderToMark -- _controllerState.idToOrder.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.MarkOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      for ((orderId, mark) <- orderToMark) {
        orderRegister(orderId).agentOrderMark = Some(mark)
      }

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case Internal.OrderIsDue(orderId) =>
      proceedWithOrders(orderId :: Nil)
      orderQueue.enqueue(orderId :: Nil)

    case Internal.ShutDown(shutDown) =>
      shutdown.delayUntil = now + config.getDuration("js7.web.server.delay-shutdown").toFiniteDuration
      shutdown.start(shutDown)

    case Internal.StillShuttingDown =>
      shutdown.onStillShuttingDown()

    case Terminated(a) if agentRegister contains a =>
      agentRegister(a).actorTerminated = true
      if (switchover.isDefined && journalTerminated && agentRegister.runningActorCount == 0) {
        val delay = shutdown.delayUntil.timeLeft
        if (delay.isPositive) {
          logger.debug(s"Sleep ${delay.pretty} after ShutDown command")
          sleep(delay)
        }
        context.stop(self)
      } else {
        shutdown.continue()
      }
  }

  private def simpleItemsToEvents(simple: VerifiedUpdateItems.Simple): Checked[View[InventoryItemEvent]] =
    simple.verifiedSimpleItems
      .flatTraverse(verifiedItemToEvents)
      .flatMap(signedEvents =>
        simple.unsignedSimpleItems
          .traverse(unsignedSimpleItemToEvent)
          .flatMap(unsignedEvents =>
            simple.delete
              .flatTraverse(id => simpleItemIdToDeletedEvent(id).map(_.toSeq))
              .map(_.view ++ signedEvents ++ unsignedEvents)))

  private def verifiedItemToEvents(verified: SignedItemVerifier.Verified[SignableSimpleItem])
  : Checked[Seq[InventoryItemEvent]] = {
    val item = verified.item
    if (item.itemRevision.isDefined)
      Left(Problem.pure("ItemRevision is not accepted here"))
    else
      Right(
        _controllerState.pathToSimpleItem.get(item.key) match {
          case None =>
            SignedItemAdded(verified.signedItem.copy(value =
              item.withRevision(Some(ItemRevision.Initial)))) :: Nil
          case Some(existing) =>
            val changed = SignedItemChanged(verified.signedItem.copy(
              value = verified.signedItem.value
                .withRevision(Some(
                  existing.itemRevision.fold(ItemRevision.Initial/*not expected*/)(_.next)))))
            val attaching = _controllerState.itemToAgentToAttachedState.get(item.key)
              .map(_.view.collect {
                case (agentPath, Attachable) => ItemAttachable(item.key, agentPath)
                case (agentPath, Attached(rev))
                  if rev != item.itemRevision && agentRequiresItem(agentPath, item.key) =>
                  ItemAttachable(item.key, agentPath)
                //case (agentPath, Detachable) => Should not happen
                //case (agentPath, Detached) => Nothing to do
              })
              .view.flatten
            Vector(changed) ++ attaching
        })
  }

  private def agentRequiresItem(agentPath: AgentPath, itemKey: InventoryItemKey): Boolean =
    // Maybe optimize ??? Changed item needs only to be attached if it is needed by an attached order
    true

  private def unsignedSimpleItemToEvent(item: UnsignedSimpleItem): Checked[UnsignedSimpleItemAddedOrChanged] =
    if (item.itemRevision.isDefined)
      Left(Problem.pure("ItemRevision is not accepted here"))
    else
      Right(
        _controllerState.pathToSimpleItem.get(item.key) match {
          case None =>
            SimpleItemAdded(item.withRevision(Some(ItemRevision.Initial)))
          case Some(existing) =>
            SimpleItemChanged(item
              .withRevision(Some(
                existing.itemRevision.fold(ItemRevision.Initial/*not expected*/)(_.next))))
        })

  private def simpleItemIdToDeletedEvent(itemKey: InventoryItemKey): Checked[Option[InventoryItemEvent]] =
    itemKey match {
      case id: OrderWatchPath =>
        Right(
          _controllerState.allOrderWatchesState.pathToOrderWatchState
            .get(id)
            .flatMap(orderWatchState =>
              !orderWatchState.delete ? (
                if (orderWatchState.isDestroyable)
                  ItemDestroyed(id)
                else
                  ItemDeletionMarked(id))))

      case _ =>
        Left(Problem.pure(s"${itemKey.companion.itemTypeName} cannot be deleted"))
    }

  private def versionedItemsToEvent(forRepo: VerifiedUpdateItems.Versioned)
  : Try[Checked[Seq[VersionedEvent]]] =
    Try(
      _controllerState.repo.itemsToEvents(
        forRepo.versionId,
        forRepo.verifiedItems.map(_.signedItem),
        forRepo.delete))

  // JournalActor's termination must be handled in any `become`-state and must lead to ControllerOrderKeeper's termination
  override def journaling = handleExceptionalMessage orElse super.journaling

  private def handleExceptionalMessage: Receive = {
    case Terminated(actor) if actor == journalActor =>
      journalTerminated = true
      if (!shuttingDown && switchover.isEmpty) logger.error("JournalActor terminated")
      if (switchover.isDefined && agentRegister.runningActorCount > 0) {
        agentRegister.values foreach {
          _.actor ! AgentDriver.Input.Terminate(noJournal = true)
        }
      } else {
        context.stop(self)
      }

    case Internal.ClusterModuleTerminatedUnexpectedly(tried) =>
      // Stacktrace has been debug-logged by Cluster
      val msg = tried match {
        case Success(Right(Completed)) => "Completed"
        case Success(Left(problem)) => problem
        case Failure(t) => t
      }
      logger.error(s"Cluster module terminated unexpectedly: $msg ")
      context.stop(self)
  }

  private def executeControllerCommand(command: ControllerCommand, commandMeta: CommandMeta): Future[Checked[ControllerCommand.Response]] =
    command match {
      case ControllerCommand.AddOrder(order) =>
        if (shuttingDown)
          Future.successful(Left(ControllerIsShuttingDownProblem))
        else if (switchover.isDefined)
          Future.successful(Left(ControllerIsSwitchingOverProblem))
        else
          addOrder(order)
            .map(_.map(added => ControllerCommand.AddOrder.Response(ignoredBecauseDuplicate = !added)))

      case ControllerCommand.AddOrders(orders) =>
        if (shuttingDown)
          Future.successful(Left(ControllerIsShuttingDownProblem))
        else if (switchover.isDefined)
          Future.successful(Left(ControllerIsSwitchingOverProblem))
        else
          addOrders(orders).map(_.map(eventId =>
            ControllerCommand.AddOrders.Response(eventId)))

      case ControllerCommand.CancelOrders(orderIds, mode) =>
        executeOrderMarkCommands(orderIds.toVector)(orderEventSource.cancel(_, mode))

      case ControllerCommand.SuspendOrders(orderIds, mode) =>
        executeOrderMarkCommands(orderIds.toVector)(orderEventSource.suspend(_, mode))

      case ControllerCommand.ResumeOrder(orderId, position, historyOutcomes) =>
        executeOrderMarkCommands(Vector(orderId))(orderEventSource.resume(_, position, historyOutcomes))

      case ControllerCommand.ResumeOrders(orderIds) =>
        executeOrderMarkCommands(orderIds.toVector)(orderEventSource.resume(_, None, None))

      case ControllerCommand.RemoveOrdersWhenTerminated(orderIds) =>
        orderIds.toVector
          .traverse(_controllerState.idToOrder.checked)
          .traverse(orders =>
            orders.traverse(order =>
              check(order.parent.isEmpty, order, CannotRemoveOrderProblem)))
          .flatten
          .map(_
            .filterNot(_.removeWhenTerminated)
            .map(orderRemovedEvent))
          .traverse(keyedEvents =>
            persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
              .map(_ => ControllerCommand.Response.Accepted))

      case ControllerCommand.ReleaseEvents(untilEventId) =>
        val userId = commandMeta.user.id
        if (!controllerConfiguration.journalConf.releaseEventsUserIds.contains(userId))
          Future(Left(UserIsNotEnabledToReleaseEventsProblem))
        else {
          val current = _controllerState.journalState.userIdToReleasedEventId.getOrElse(userId, EventId.BeforeFirst)
          if (untilEventId < current)
            Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = untilEventId, currentUntilEventId = current)))
          else
            persist(JournalEventsReleased(userId, untilEventId)) { (_, updatedState) =>
              _controllerState = updatedState
              Right(ControllerCommand.Response.Accepted)
            }
        }

      case ControllerCommand.NoOperation(maybeDuration) =>
        // NoOperation completes only after ControllerOrderKeeper has become ready (can be used to await readiness)
        Task.pure(Right(ControllerCommand.Response.Accepted))
          .delayExecution(maybeDuration getOrElse 0.s)
          .runToFuture

      case _: ControllerCommand.EmergencyStop | _: ControllerCommand.Batch =>       // For completeness. RunningController has handled the command already
        Future.successful(Left(Problem.pure("THIS SHOULD NOT HAPPEN")))  // Never called

      case ControllerCommand.TakeSnapshot =>
        import controllerConfiguration.akkaAskTimeout  // We need several seconds or even minutes
        intelliJuseImport(akkaAskTimeout)
        (journalActor ? JournalActor.Input.TakeSnapshot)
          .mapTo[JournalActor.Output.SnapshotTaken.type]
          .map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.ClusterSwitchOver =>
        clusterSwitchOver(restart = true)

      case shutDown: ControllerCommand.ShutDown =>
        shutDown.clusterAction match {
          case Some(ControllerCommand.ShutDown.ClusterAction.Switchover) =>
            clusterSwitchOver(restart = shutDown.restart)

          case Some(ControllerCommand.ShutDown.ClusterAction.Failover) =>
            // TODO ClusterState.Coupled !
            shutdown.start(shutDown)
            Future.successful(Right(ControllerCommand.Response.Accepted))

          case None =>
            clusterNode.shutDownThisNode
              .flatTap {
                case Right(Completed) => Task { self ! Internal.ShutDown(shutDown) }
                case _ => Task.unit
              }
              .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
              .runToFuture
        }

      case ControllerCommand.EmitTestEvent =>
        persist(ControllerTestEvent, async = true) { (_, updatedState) =>
          _controllerState = updatedState
          Right(ControllerCommand.Response.Accepted)
        }

      case _ =>
        // Handled by ControllerCommandExecutor
        Future.failed(new NotImplementedError)
    }

  private def orderRemovedEvent(order: Order[Order.State]): KeyedEvent[OrderCoreEvent] =
    order.id <-: (
      if (order.isState[Order.IsTerminated])
        OrderRemoved
      else
        OrderRemoveMarked)

  private def executeOrderMarkCommands(orderIds: Vector[OrderId])(toEvent: OrderId => Checked[Option[OrderActorEvent]])
  : Future[Checked[ControllerCommand.Response]] =
    if (orderIds.distinct.sizeIs < orderIds.size)
      Future.successful(Left(Problem.pure("OrderIds must be unique")))
    else
      orderIds.traverse(_controllerState.idToOrder.checked) match {
        case Left(problem) =>
          Future.successful(Left(problem))

        case Right(orders) =>
          orders
            .traverse(order => toEvent(order.id).map(_.map(order.id <-: _)))
            .map(_.flatten)
            .traverse(keyedEvents =>
              // Event may be inserted between events coming from Agent
              persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents))
            .map(_.map(_ => ControllerCommand.Response.Accepted))
      }

  private def persistItemAndVersionedEvents(keyedEvents: Seq[AnyKeyedEvent]): Future[Checked[Completed]] = {
    // Precheck events
    _controllerState.applyEvents(keyedEvents) match {
      case Left(prblm @ Problem.Combined(Seq(_, problem: DuplicateKey))) =>
        logger.debug(prblm.toString)
        Future.successful(Left(problem))

      case Left(problem) =>
        Future.successful(Left(problem))

      case Right(_) =>
        persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
          .map(_ => Right(Completed))
    }
  }

  private def logEvent(event: Event): Unit =
    event match {
      case e: InventoryItemEvent => logger.trace(s"${e.key} ${e.getClass.scalaName}")
      case VersionAdded(version) => logger.trace(s"Version '${version.string}' added")
      case e: VersionedItemEvent => logger.trace(s"${e.path} ${e.getClass.scalaName}")
      case _ =>
    }

  private def registerAgent(agent: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId): AgentEntry = {
    val actor = watch(actorOf(
      AgentDriver.props(agent.path, agent.uri, agentRunId, eventId = eventId, agentDriverConfiguration, controllerConfiguration,
        journalActor = journalActor),
      encodeAsActorName("Agent-" + agent.path)))
    val entry = AgentEntry(agent, actor)
    agentRegister.insert(agent.path -> entry)
    entry
  }

  private def addOrder(order: FreshOrder): Future[Checked[Boolean]] =
    suppressOrderIdCheckFor match {
      case Some(order.id.string) =>  // Test only
        addOrderWithUncheckedId(order)

      case _ =>
        order.id.checkedNameSyntax match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(_) => addOrderWithUncheckedId(order)
        }
    }

  private def addOrderWithUncheckedId(freshOrder: FreshOrder): Future[Checked[Boolean]] =
    _controllerState.idToOrder.get(freshOrder.id) match {
      case Some(_) =>
        logger.debug(s"Discarding duplicate added Order: $freshOrder")
        Future.successful(Right(false))

      case None =>
        _controllerState.repo.pathTo[Workflow](freshOrder.workflowPath) match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(workflow) =>
            workflow.orderRequirements.checkArguments(freshOrder.arguments) match {
              case Left(problem) => Future.successful(Left(problem))
              case Right(()) =>
                val events = freshOrder.toOrderAdded(workflow.id.versionId) :: Nil
                persistTransactionAndSubsequentEvents(events) { (stamped, updatedState) =>
                  handleEvents(stamped, updatedState)
                  Right(true)
                }
                .flatMap(o => testAddOrderDelay.runToFuture.map(_ => o))  // test only
            }
        }
    }

  private def addOrders(freshOrders: Seq[FreshOrder]): Future[Checked[EventId]] =
    freshOrders.checkUniqueness(_.id) match {
      case Left(problem) => Future.successful(Left(problem))
      case _ =>
        freshOrders.toVector
          .filterNot(o => _controllerState.idToOrder contains o.id)  // Ignore known orders
          .traverse(o => _controllerState.repo.pathTo[Workflow](o.workflowPath).map(o.->))
          .traverse { ordersAndWorkflows =>
            val events = for ((order, workflow) <- ordersAndWorkflows) yield
              order.id <-: OrderAdded(workflow.id/*reuse*/, order.arguments, order.scheduledFor)
            persistTransaction(events) { (stamped, updatedState) =>
              handleEvents(stamped, updatedState)
              // Emit subsequent events later for earlier addOrders response (and smaller event chunk)
              orderQueue.enqueue(freshOrders.view.map(_.id))
              updatedState.eventId
            }
          }
    }

  private def persistTransactionAndSubsequentEvents[A](keyedEvents: Seq[KeyedEvent[Event]])
    (callback: (Seq[Stamped[KeyedEvent[Event]]], ControllerState) => A)
  : Future[A] =
    persistTransaction(keyedEvents ++ subsequentEvents(keyedEvents))(callback)

  private def subsequentEvents(keyedEvents: Seq[KeyedEvent[Event]]): Seq[KeyedEvent[Event]] =
    delayOrderRemoved(
      new ControllerStateExecutor(_controllerState)
        .applyEventsAndReturnSubsequentEvents(keyedEvents))

  private def nextOrderEvents(orderIds: Seq[OrderId]): Seq[KeyedEvent[OrderCoreEvent]] =
    delayOrderRemoved(
      new ControllerStateExecutor(_controllerState).nextOrderEventsByOrderId(orderIds).toVector)

  private def handleEvents(stampedEvents: Seq[Stamped[KeyedEvent[Event]]], updatedState: ControllerState): Unit = {
    val itemIds = mutable.Buffer.empty[InventoryItemKey]
    val orderIds = mutable.Buffer.empty[OrderId]
    for (stamped <- stampedEvents) {
      val keyedEvent = stamped.value
      keyedEvent match {
        case KeyedEvent(orderId: OrderId, _: OrderEvent) =>
          orderIds += orderId
          orderIds ++= handleOrderEvent(keyedEvent.asInstanceOf[KeyedEvent[OrderEvent]])
          _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow

        case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
          _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow
          itemIds += event.key
          handleItemEvent(event)

        case _ =>
        _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow
      }
    }
    _controllerState = updatedState  // Reduce memory usage (they are equal)
    itemIds.distinct foreach proceedWithItem
    proceedWithOrders(orderIds.distinct)
  }

  private def handleItemEvent(event: InventoryItemEvent): Unit = {
    logEvent(event)
    event match {
      case SimpleItemAdded(agentRef: AgentRef) =>
        val entry = registerAgent(agentRef, agentRunId = None, eventId = EventId.BeforeFirst)
        entry.actor ! AgentDriver.Input.StartFetchingEvents

      case SimpleItemChanged(agentRef: AgentRef) =>
        agentRegister.update(agentRef)
        agentRegister(agentRef.path).reconnect()

      case _ =>
    }
  }

  private def proceedWithItem(itemKey: InventoryItemKey): Unit =
    itemKey match {
      case agentPath: AgentPath =>
        // TODO Handle AgentRef here: agentEntry .actor ! AgentDriver.Input.StartFetchingEvents ...

      case path: OrderWatchPath =>
        for (orderWatchState <- _controllerState.allOrderWatchesState.pathToOrderWatchState.get(path)) {
          import orderWatchState.orderWatch
          for ((agentPath, attachedState) <- orderWatchState.agentPathToAttachedState) {
            // TODO Does nothing if Agent is added later! (should be impossible, anyway)
            for (agentEntry <- agentRegister.get(agentPath)) {
              attachedState match {
                case Attachable =>
                  agentEntry.actor ! AgentDriver.Input.AttachUnsignedItem(orderWatch)

                case Detachable =>
                  agentEntry.actor ! AgentDriver.Input.DetachItem(orderWatch.key)

                case _ =>
              }
            }
          }
        }

      case itemId: SignableSimpleItemPath =>
        for (agentToAttachedState <- _controllerState.itemToAgentToAttachedState.get(itemId)) {
          for ((agentPath, attachedState) <- agentToAttachedState) {
            // TODO Does nothing if Agent is added later! (should be impossible, anyway)
            for (agentEntry <- agentRegister.get(agentPath)) {
              attachedState match {
                case Attachable =>
                  for (signedItem <- _controllerState.idToSignedSimpleItem.get(itemId)) {
                    agentEntry.actor ! AgentDriver.Input.AttachSignedItem(signedItem)
                  }

                case Detachable =>
                  agentEntry.actor ! AgentDriver.Input.DetachItem(itemId)

                case _ =>
              }
            }
          }
        }

      case _ =>
    }

  private def handleOrderEvent(keyedEvent: KeyedEvent[OrderEvent]): Seq[OrderId] = {
    val KeyedEvent(orderId, event) = keyedEvent

    updateOrderEntry(orderId, event)

    event match {
      case _: OrderAdded =>
        orderId :: Nil

      case _ =>
        _controllerState.idToOrder.get(orderId) match {
          case None =>
            logger.error(s"Unknown OrderId in event ${orderId <-: event}")
            Nil

          case Some(order) =>
            val checkedFollowUps = orderEventHandler.handleEvent(orderId <-: event)
            val dependentOrderIds = mutable.Buffer.empty[OrderId]
            for (followUps <- checkedFollowUps.onProblem(p => logger.error(p))) {  // TODO OrderBroken on error?
              followUps foreach {
                case _: FollowUp.Processed if order.isAttached =>

                case FollowUp.AddChild(childOrder) =>
                  dependentOrderIds += childOrder.id

                case FollowUp.AddOffered(offeredOrder) =>
                  dependentOrderIds += offeredOrder.id

                case FollowUp.Remove(removeOrderId) =>
                  orderRegister -= removeOrderId

                case _: FollowUp.Processed =>
              }
            }

            dependentOrderIds.toSeq ++
              (_controllerState.idToOrder.contains(orderId) ? order.id)
        }
    }
  }

  private def updateOrderEntry(orderId: OrderId, event: OrderEvent): Unit = {
    val orderEntry = orderRegister.getOrElseUpdate(orderId, new OrderEntry(now))
    orderEntry.lastUpdatedAt = now
    event match {
      case _: OrderAttachable | _: OrderDetachable =>
        orderEntry.triedToAttached = false

      case OrderDetached =>
        orderEntry.isDetaching = false

      case _ =>
    }
  }

  private def proceedWithOrders(orderIds: Iterable[OrderId]): Unit =
    if (!shuttingDown && switchover.isEmpty) {
      orderIds foreach proceedWithOrder
    }

  private def proceedWithOrder(orderId: OrderId): Unit =
    for (order <- _controllerState.idToOrder.get(orderId)) {
      for (mark <- order.mark) {
        if (order.isAttached && !orderMarkTransferredToAgent(order.id).contains(mark)) {
          // On Recovery, MarkOrder is sent again, because orderEntry.agentOrderMark is lost
          for ((_, agentEntry) <- checkedWorkflowAndAgentEntry(order).onProblem(p => logger.error(p))) {  // TODO OrderBroken on error?
            // CommandQueue filters multiple equal MarkOrder because we may send multiple due to asynchronous excecution
            agentEntry.actor ! AgentDriver.Input.MarkOrder(order.id, mark)
          }
        }
      }
      order.attachedState match {
        case None |
             Some(_: Order.Attaching) => proceedWithOrderOnController(order)
        case Some(_: Order.Attached)  =>
        case Some(_: Order.Detaching) => detachOrderFromAgent(order.id)
      }
    }

  private def proceedWithOrderOnController(order: Order[Order.State]): Unit =
    order.state match {
      case _: Order.IsFreshOrReady if order.isProcessable =>
        val freshOrReady = order.castState[Order.IsFreshOrReady]
        instruction(order.workflowPosition) match {
          case _: Execute => tryAttachOrderToAgent(freshOrReady)
          case _ =>
        }

      case _: Order.Offering =>
        for (awaitingOrderId <- orderEventHandler.offeredToAwaitingOrder(order.id);
             awaitingOrder <- _controllerState.idToOrder.checked(awaitingOrderId).onProblem(p => logger.warn(p.toString));
             _ <- awaitingOrder.checkedState[Order.Awaiting].onProblem(p => logger.error(p.toString)))  // TODO OrderBroken on error?
        {
          //not implemented —— proceedWithOrderOnController(awaitingOrder)
        }

      case _ =>
    }

  private def delayOrderRemoved[E <: Event](keyedEvents: Seq[KeyedEvent[E]]): Seq[KeyedEvent[E]] =
    if (!removeOrderDelay.isPositive)
      keyedEvents
    else
      keyedEvents.filter {
        case KeyedEvent(orderId: OrderId, OrderRemoved) =>
          orderRegister.get(orderId).fold(false) { orderEntry =>
            val delay = orderEntry.lastUpdatedAt + removeOrderDelay - now
            !delay.isPositive || {
              orderRegister(orderId).timer := scheduler.scheduleOnce(delay) {
                self ! Internal.OrderIsDue(orderId)
              }
              false
            }
            // When recovering, proceedWithOrderOnController may emit the same event multiple times,
            // for example OrderJoined for each parent and child order.
            // These events are collected and with actor message Internal.AfterProceedEventsAdded reduced to one.
          }
        case _ => true
      }

  private def tryAttachOrderToAgent(order: Order[Order.IsFreshOrReady]): Unit =
    for ((signedWorkflow, agentEntry) <- checkedWorkflowAndAgentEntry(order)
      .onProblem(p => logger.error(p withPrefix "tryAttachOrderToAgent:"))
    ) {  // TODO OrderBroken on error?
      if (order.isAttaching) {
        val orderEntry = orderRegister(order.id)
        if (!orderEntry.triedToAttached) {
          val jobResources = signedWorkflow.value.referencedJobResourcePaths
            .flatMap(_controllerState.idToSignedSimpleItem.get)
          for (signedItem <- jobResources ++ View(signedWorkflow)) {
            val item = signedItem.value
            val attachedState = _controllerState
              .itemToAttachedState(item.key, item.itemRevision, agentEntry.agentPath)
            if (attachedState == Detached || attachedState == Attachable) {
              agentEntry.actor ! AgentDriver.Input.AttachSignedItem(signedItem)
            }
          }

          orderEntry.triedToAttached = true
          agentEntry.actor ! AgentDriver.Input.AttachOrder(order, agentEntry.agentPath)
        }
      }
    }

  private def checkedWorkflowAndAgentEntry(order: Order[Order.State]): Checked[(Signed[Workflow], AgentEntry)] =
    for {
      signedWorkflow <- _controllerState.repo.idToSigned[Workflow](order.workflowId)
      job <- signedWorkflow.value.checkedWorkflowJob(order.position)
      agentEntry <- agentRegister.checked(job.agentPath)
    } yield (signedWorkflow, agentEntry)

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    for (orderEntry <- orderRegister.get(orderId)) {
      if (!orderEntry.isDetaching) {
        _controllerState.idToOrder.checked(orderId)
          .flatMap(_.detaching)
          .onProblem(p => logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
          .foreach { agentPath =>
            agentRegister.get(agentPath) match {
              case None => logger.error(s"detachOrderFromAgent '$orderId': Unknown $agentPath")
              case Some(a) =>
                a.actor ! AgentDriver.Input.DetachOrder(orderId)
                orderEntry.isDetaching = true
            }
          }
      }
    }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    _controllerState.repo.idTo[Workflow](workflowPosition.workflowId).orThrow
      .instruction(workflowPosition.position)

  private def clusterSwitchOver(restart: Boolean)
  : Future[Checked[ControllerCommand.Response.Accepted.type]] =
    if (switchover.isDefined)
      Future.successful(Left(Problem("Already switching over")))
    else
      Task {
        new Switchover(restart = restart)
      } .bracketCase { so =>
          switchover = Some(so)
          so.start()
            .materialize.flatTap {
              case Success(Right(_)) => Task.unit  // this.switchover is left for postStop
              case _ => Task {
                switchover = None  // Asynchronous!
              }
            }.dematerialize
        } ((so, exitCase) =>
          Task {
            logger.debug(s"SwitchOver => $exitCase")
            so.close()
          })
        .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
        .runToFuture

  private def orderMarkTransferredToAgent(orderId: OrderId): Option[OrderMark] =
    orderRegister.get(orderId).flatMap(_.agentOrderMark)

  override def toString = "ControllerOrderKeeper"
}

private[controller] object ControllerOrderKeeper
{
  private object ControllerIsShuttingDownProblem extends Problem.ArgumentlessCoded
  private object ControllerIsSwitchingOverProblem extends Problem.ArgumentlessCoded

  private val logger = Logger(getClass)

  object Input {
    final case class Start(recovered: Recovered[ControllerState])
  }

  sealed trait Command
  object Command {
    final case class Execute(command: ControllerCommand, meta: CommandMeta) extends Command
    final case class VerifiedUpdateItemsCmd(verifiedUpdateRepo: VerifiedUpdateItems) extends Command
  }

  sealed trait Reponse
  object Response {
    final case class ForAddOrder(created: Checked[Boolean])
  }

  private object Internal {
    final case class JournalIsReady(journalHeader: Try[JournalHeader])
    case object ContinueWithNextOrderEvents extends DeadLetterSuppression
    final case class OrderIsDue(orderId: OrderId) extends DeadLetterSuppression
    final case class Activated(recovered: Try[Unit])
    final case class ClusterModuleTerminatedUnexpectedly(tried: Try[Checked[Completed]]) extends DeadLetterSuppression
    final case class Ready(outcome: Checked[Completed])
    case object StillShuttingDown extends DeadLetterSuppression
    final case class ShutDown(shutdown: ControllerCommand.ShutDown)
  }

  private implicit final class RichIdToOrder(private val idToOrder: Map[OrderId, Order[Order.State]]) extends AnyVal {
    def checked(orderId: OrderId) = idToOrder.get(orderId).toChecked(UnknownOrderProblem(orderId))
  }

  private class AgentRegister extends ActorRegister[AgentPath, AgentEntry](_.actor) {
    override def insert(kv: (AgentPath, AgentEntry)) = super.insert(kv)
    override def -=(a: ActorRef) = super.-=(a)

    def update(agentRef: AgentRef): Unit = {
      val oldEntry = apply(agentRef.path)
      super.update(agentRef.path -> oldEntry.copy(agentRef = agentRef))
    }

    def runningActorCount = values.count(o => !o.actorTerminated)
  }

  private case class AgentEntry(
    agentRef: AgentRef,
    actor: ActorRef,
    var actorTerminated: Boolean = false)
  {
    def agentPath = agentRef.path

    def reconnect()(implicit sender: ActorRef): Unit =
      actor ! AgentDriver.Input.ChangeUri(uri = agentRef.uri)
  }

  private class OrderEntry(now: MonixDeadline)
  {
    var triedToAttached = false
    var isDetaching = false
    var lastUpdatedAt: MonixDeadline = now
    var agentOrderMark = none[OrderMark]
    val timer = SerialCancelable()
  }

  object ControllerReadyTestIncident
}
