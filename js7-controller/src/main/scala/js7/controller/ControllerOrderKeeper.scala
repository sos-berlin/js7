package js7.controller

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import cats.effect.SyncIO
import cats.instances.either._
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.traverse._
import java.time.ZoneId
import js7.agent.data.event.AgentControllerEvent
import js7.base.crypt.Signed
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.common.akkautils.SupervisorStrategies
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.Logger.ops._
import js7.controller.ControllerOrderKeeper._
import js7.controller.agent.{AgentDriver, AgentDriverConfiguration}
import js7.controller.cluster.Cluster
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.agent.{AgentEventIdEvent, AgentSnapshot}
import js7.controller.data.events.ControllerAgentEvent.AgentReady
import js7.controller.data.events.ControllerEvent
import js7.controller.data.events.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.controller.data.{ControllerCommand, ControllerState}
import js7.controller.problems.ControllerIsNotYetReadyProblem
import js7.controller.repo.{RepoCommandExecutor, VerifiedUpdateRepo}
import js7.core.command.CommandMeta
import js7.core.common.ActorRegister
import js7.core.event.journal.recover.Recovered
import js7.core.event.journal.{JournalActor, MainJournalingActor}
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.{AgentRef, AgentRefPath, AgentRunId}
import js7.data.cluster.ClusterState
import js7.data.crypt.InventoryItemVerifier
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, JournalHeader, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.execution.workflow.{OrderEventHandler, OrderEventSource}
import js7.data.item.RepoEvent.{ItemAdded, ItemChanged, ItemDeleted, ItemEvent, VersionAdded}
import js7.data.item.{IntenvoryItems, InventoryItem, RepoChange, RepoEvent, TypedPath}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderAttachable, OrderBroken, OrderCancelMarked, OrderResumeMarked, OrderSuspendMarked, OrderTransferredToAgent, OrderTransferredToController}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderMark}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.SerialCancelable
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
import shapeless.tag.@@

/**
  * @author Joacim Zschimmer
  */
final class ControllerOrderKeeper(
  stopped: Promise[ControllerTermination],
  protected val journalActor: ActorRef @@ JournalActor.type,
  cluster: Cluster[ControllerState],
  controllerConfiguration: ControllerConfiguration,
  itemVerifier: InventoryItemVerifier[InventoryItem],
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

  private val agentDriverConfiguration = AgentDriverConfiguration.fromConfig(config, controllerConfiguration.journalConf).orThrow
  private var _controllerState: ControllerState = ControllerState.Undefined

  private val repoCommandExecutor = new RepoCommandExecutor(itemVerifier)
  private val agentRegister = new AgentRegister
  private val orderRegister = mutable.HashMap[OrderId, OrderEntry]()
  private val recoveredJournalHeader = SetOnce[JournalHeader]
  private val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  private val testAddOrderDelay = config.optionAs[FiniteDuration]("js7.TEST-ONLY.add-order-delay").fold(Task.unit)(Task.sleep)
  private var journalTerminated = false

  private object shutdown {
    val since = SetOnce[Deadline]
    private val shutDown = SetOnce[ControllerCommand.ShutDown]
    private val stillShuttingDownCancelable = SerialCancelable()
    private var terminatingAgentDrivers = false
    private var takingSnapshot = false
    private var snapshotTaken = false
    private var terminatingJournal = false

    def shuttingDown = since.isDefined

    def restart = shutDown.fold(false)(_.restart)

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
              .runToFuture.onComplete { tried =>
                tried match {
                  case Success(Right(Completed)) =>
                  case other => logger.error(s"While shutting down: $other")
                }
                journalActor ! JournalActor.Input.Terminate
              }
          }
        }
      }
  }
  import shutdown.shuttingDown

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
      cluster.switchOver   // Will terminate `cluster`, letting ControllerOrderKeeper terminate
        .map(_.map { case Completed =>
          journalActor ! JournalActor.Input.Terminate
          Completed
        })

    def close() = stillSwitchingOverSchedule.cancel()
  }

  private object afterProceedEvents {
    private val events = mutable.Buffer[KeyedEvent[OrderEvent]]()

    def persistAndHandleLater(keyedEvent: KeyedEvent[OrderEvent]): Unit = {
      val first = events.isEmpty
      events += keyedEvent
      if (first) {
        self ! Internal.AfterProceedEventsAdded
      }
    }

    def persistThenHandleEvents(): Unit = {
      // Eliminate duplicate events like OrderJoined, which may be emitted by parent and child orders when recovering
      persistMultiple(events.distinct)(handleOrderEvents)
      events.clear()
    }

    override def toString = events.mkString(", ")  // For debugging
  }

  watch(journalActor)

  override def postStop() =
    try {
      cluster.stop()
      shutdown.close()
      switchover foreach { _.close() }
    } finally {
      logger.debug("Stopped" + shutdown.since.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))
      stopped.success(
        if (switchover.exists(_.restart)) ControllerTermination.Restart
        else ControllerTermination.Terminate(restart = shutdown.restart))
      super.postStop()
    }

  protected def snapshots = Future.successful(Nil)

  def receive = {
    case Input.Start(recovered) =>
      assertActiveClusterState(recovered)
      recover(recovered)

      become("inhibitingActivationOfOtherClusterNode")(inhibitingActivationOfOtherClusterNode)
      unstashAll()
      // TODO Inhibit activation of peer while recovering a long time
      cluster.beforeJournalingStarted
        .map(_.orThrow)
        .map((_: Completed) => recovered)
        .materialize
        .map(Internal.OtherClusterNodeActivationInhibited.apply)
        .runToFuture
        .pipeTo(self)

    case msg => notYetReady(msg)
  }

  private def assertActiveClusterState(recovered: Recovered[ControllerState]): Unit =
    for (clusterState <- recovered.recoveredState.map(_.clusterState)) {
      val ownId = controllerConfiguration.nodeId
      if (clusterState != ClusterState.Empty && !clusterState.isNonEmptyActive(ownId))
        throw new IllegalStateException(
          s"Controller has recovered from Journal but is not the active node in ClusterState: id=$ownId, failedOver=$clusterState")
    }

  private def recover(recovered: Recovered[ControllerState]): Unit = {
    for (controllerState <- recovered.recoveredState) {
      if (controllerState.controllerMetaState.controllerId != controllerConfiguration.controllerId)
        throw Problem(s"Recovered controllerId='${controllerState.controllerMetaState.controllerId}' differs from configured controllerId='${controllerConfiguration.controllerId}'")
          .throwable
      this._controllerState = controllerState
      //controllerMetaState = controllerState.controllerMetaState.copy(totalRunningTime = recovered.totalRunningTime)
      for (agentRef <- controllerState.repo.currentItems collect { case o: AgentRef => o }) {
        val agentSnapshot = controllerState.pathToAgentSnapshot.getOrElse(agentRef.path,
          AgentSnapshot(agentRef.path, None, EventId.BeforeFirst))
        val e = registerAgent(agentRef, agentSnapshot.agentRunId, eventId = agentSnapshot.eventId)
        // Send an extra RegisterMe here, to be sure JournalActor has registered the AgentDriver when a snapshot is taken
        // TODO Fix fundamentally the race condition with JournalActor.Input.RegisterMe
        journalActor.tell(JournalActor.Input.RegisterMe, e.actor)
      }

      persistedEventId = controllerState.eventId
    }
  }

  private def inhibitingActivationOfOtherClusterNode: Receive = {
    case Internal.OtherClusterNodeActivationInhibited(Failure(t)) =>
      logger.error(s"Activation of this cluster node failed because the other cluster node reports: ${t.toStringWithCauses}")
      if (t.getStackTrace.nonEmpty) logger.debug(t.toStringWithCauses, t)
      throw t.appendCurrentStackTrace

    case Internal.OtherClusterNodeActivationInhibited(Success(recovered)) =>
      // Send an extra RegisterMe here, to be sure JournalActor has registered the ClusterState actor when a snapshot is taken
      // TODO Fix fundamentally the race condition with JournalActor.Input.RegisterMe
      journalActor.tell(JournalActor.Input.RegisterMe, cluster.journalingActor)
      recovered.startJournalAndFinishRecovery(journalActor)
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

      case Command.VerifiedUpdateRepoCmd(_) =>
        logger.warn(s"$ControllerIsNotYetReadyProblem: VerifiedUpdateRepoCmd")
        sender() ! Left(ControllerIsNotYetReadyProblem)

      case cmd: Command =>
        logger.warn(s"$ControllerIsNotYetReadyProblem: $cmd")
        sender() ! Status.Failure(ControllerIsNotYetReadyProblem.throwable)

      case _ => stash()
    }

  private def journalIsStarting: Receive = {
    case Recovered.Output.JournalIsReady(journalHeader) =>
      recoveredJournalHeader := journalHeader
      become("becomingReady")(becomingReady)  // `become` must be called early, before any persist!

      persistMultiple(
        (!_controllerState.controllerMetaState.isDefined ?
          (NoKey <-: ControllerEvent.ControllerInitialized(controllerConfiguration.controllerId, journalHeader.startedAt))
        ) ++ Some(NoKey <-: ControllerEvent.ControllerReady(ZoneId.systemDefault.getId, totalRunningTime = journalHeader.totalRunningTime))
      ) { (_, updatedControllerState) =>
        _controllerState = updatedControllerState
        cluster.afterJounalingStarted
          .materializeIntoChecked
          .runToFuture
          .map(Internal.Ready.apply)
          .pipeTo(self)
      }

      // Proceed order before starting AgentDrivers, so AgentDrivers may match recovered OrderIds with Agent's OrderIds
      // Any ordering when continuing orders???
      _controllerState.idToOrder.keys foreach proceedWithOrder // May persist events later!
      afterProceedEvents.persistThenHandleEvents()  // Persist and handle before Internal.Ready
      if (persistedEventId > EventId.BeforeFirst) {  // Recovered?
        logger.info(s"${_controllerState.idToOrder.size} Orders, ${_controllerState.repo.typedCount[Workflow]} Workflows and ${_controllerState.repo.typedCount[AgentRef]} AgentRefs recovered")
      }

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
      cluster.onTerminatedUnexpectedly.runToFuture onComplete { tried =>
        self ! Internal.ClusterModuleTerminatedUnexpectedly(tried)
      }
      become("Ready")(ready orElse handleExceptionalMessage)
      unstashAll()

    case _ =>
      // stash Command too, after ControllerReady event and cluster node has been initialized (see above)
      stash()
  }

  private def ready: Receive = {
    case Internal.AfterProceedEventsAdded =>
      afterProceedEvents.persistThenHandleEvents()

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

    case Command.VerifiedUpdateRepoCmd(verifiedUpdateRepo) =>
      val sender = this.sender()
      val t = now
      (Try(
        _controllerState.repo.itemToEvents(
          verifiedUpdateRepo.versionId,
          verifiedUpdateRepo.verifiedItem.map(_.signedItem),
          verifiedUpdateRepo.delete)
      ) match {
        // TODO Duplicate code
        case Failure(t) => Future.failed(t)
        case Success(Left(problem)) => Future.successful(Left(problem))
        case Success(Right(repoEvents)) =>
          applyRepoEvents(repoEvents)
            .traverse((effect: SyncIO[Future[Completed]]) =>
              (SyncIO(if (t.elapsed > 1.s)
                logger.debug(s"RepoEvents calculated - ${Stopwatch.itemsPerSecondString(t.elapsed, repoEvents.size, "items")}")
              ) >>
                effect
              ).unsafeRunSync())  // Persist events!
      }) onComplete {
        case Failure(t) => sender ! Status.Failure(t)
        case Success(response) => sender ! (response: Checked[Completed])
      }

    case Command.AddOrder(order) =>
      if (shuttingDown)
        sender() ! Status.Failure(ControllerIsShuttingDownProblem.throwable)
      else if (switchover.isDefined)
        sender() ! Status.Failure(ControllerIsSwitchingOverProblem.throwable)
      else
        addOrder(order) map Response.ForAddOrder.apply pipeTo sender()

    case Command.AddOrders(orders) =>
      logger.debug(s"Adding ${orders.size} orders")
      if (shuttingDown)
        sender() ! Status.Failure(ControllerIsShuttingDownProblem.throwable)
      else if (switchover.isDefined)
        sender() ! Status.Failure(ControllerIsSwitchingOverProblem.throwable)
      else
        addOrders(orders).pipeTo(sender())

    case AgentDriver.Output.EventsFromAgent(stampeds, completedPromise) =>
      val agentEntry = agentRegister(sender())
      import agentEntry.agentRefPath
      var lastAgentEventId: Option[EventId] = None
      var controllerStamped: Seq[Timestamped[Event]] =
        stampeds.view.flatMap {
          case Stamped(agentEventId, timestamp, keyedEvent) =>
            // TODO Event vor dem Speichern mit Order.applyEvent ausprobieren! Bei Fehler ignorieren?
            lastAgentEventId = Some(agentEventId)
            keyedEvent match {
              case KeyedEvent(_, _: OrderCancelMarked | _: OrderSuspendMarked | _: OrderResumeMarked) =>
                // We (the Controller) have emitted the same event
                None

              case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                val ownEvent = event match {
                  case _: OrderEvent.OrderAttached => OrderTransferredToAgent(agentRefPath) // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                  case _ => event
                }
                Some(Timestamped(orderId <-: ownEvent, Some(timestamp)))

              case KeyedEvent(_: NoKey, AgentControllerEvent.AgentReadyForController(timezone, _)) =>
                Some(Timestamped(agentEntry.agentRefPath <-: AgentReady(timezone), Some(timestamp)))

              case _ =>
                logger.warn(s"Unknown event received from ${agentEntry.agentRefPath}: $keyedEvent")
                None
            }
        }.toVector
      controllerStamped ++= lastAgentEventId.map(agentEventId => Timestamped(agentRefPath <-: AgentEventIdEvent(agentEventId)))

      completedPromise.completeWith(
        persistTransactionTimestamped(controllerStamped, async = true, alreadyDelayed = agentDriverConfiguration.eventBufferDelay) {
          (stampedEvents, updatedState) =>
            // Inhibit OrderAdded, OrderFinished, OrderJoined(?), OrderAttachable and others ???
            //  Agent does not send these events, but just in case.
            handleOrderEvents(
              stampedEvents.collect {
                case stamped @ Stamped(_, _, KeyedEvent(_: OrderId, _: OrderEvent)) =>
                  stamped.asInstanceOf[Stamped[KeyedEvent[OrderEvent]]]
              },
              updatedState)
            Completed
        })

    case AgentDriver.Output.OrdersDetached(orderIds) =>
      val unknown = orderIds -- _controllerState.idToOrder.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.DetachOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      persistMultipleAsync((orderIds -- unknown).map(_ <-: OrderTransferredToController))(handleOrderEvents)

    case AgentDriver.Output.OrdersMarked(orderToMark) =>
      val unknown = orderToMark -- _controllerState.idToOrder.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.MarkOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      for ((orderId, mark) <- orderToMark) {
        if (!orderRegister.contains(orderId)) orderRegister.put(orderId, new OrderEntry)
        orderRegister(orderId).agentOrderMark = Some(mark)
      }

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case Internal.ShutDown(shutDown) =>
      shutdown.start(shutDown)

    case Internal.StillShuttingDown =>
      shutdown.onStillShuttingDown()

    case Terminated(a) if agentRegister contains a =>
      agentRegister(a).actorTerminated = true
      if (switchover.isDefined && journalTerminated && agentRegister.runningActorCount == 0) {
        context.stop(self)
      } else {
        shutdown.continue()
      }
  }

  // JournalActor's termination must be handled in any `become`-state and must lead to ControllerOrderKeeper's termination
  override def journaling = handleExceptionalMessage orElse super.journaling

  private def handleExceptionalMessage: Receive = {
    case Terminated(`journalActor`) =>
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

      case ControllerCommand.CancelOrder(orderId, mode) =>
        executeOrderMarkCommand(orderId)(orderEventSource(_controllerState).cancel(_, mode))

      case ControllerCommand.SuspendOrder(orderId) =>
        executeOrderMarkCommand(orderId)(orderEventSource(_controllerState).suspend(_))

      case ControllerCommand.ResumeOrder(orderId) =>
        executeOrderMarkCommand(orderId)(orderEventSource(_controllerState).resume(_))

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

      case cmd: ControllerCommand.ReplaceRepo =>
        intelliJuseImport(catsStdInstancesForFuture)  // For traverse
        Try(
          repoCommandExecutor.replaceRepoCommandToEvents(_controllerState.repo, cmd, commandMeta)
            .runToFuture
            .await/*!!!*/(controllerConfiguration.akkaAskTimeout.duration))  // May throw TimeoutException
        match {
          case Failure(t) => Future.failed(t)
          case Success(checkedRepoEvents) =>
            checkedRepoEvents
              .flatMap(applyRepoEvents)
              .traverse((_: SyncIO[Future[Completed]])
                .unsafeRunSync()  // Persist events!
                .map(_ => ControllerCommand.Response.Accepted))
        }

      case cmd: ControllerCommand.UpdateRepo =>
        Try(
          repoCommandExecutor.updateRepoCommandToEvents(_controllerState.repo, cmd, commandMeta)
            .runToFuture
            .await/*!!!*/(controllerConfiguration.akkaAskTimeout.duration))  // May throw TimeoutException
        match {
          case Failure(t) => Future.failed(t)
          case Success(checkedRepoEvents) =>
            checkedRepoEvents
              .flatMap(applyRepoEvents)
              .traverse((_: SyncIO[Future[Completed]])
                .unsafeRunSync()  // Persist events!
                .map(_ => ControllerCommand.Response.Accepted))
        }

      case ControllerCommand.NoOperation =>
        // NoOperation completes only after ControllerOrderKeeper has become ready (can be used to await readiness)
        Future.successful(Right(ControllerCommand.Response.Accepted))

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
            cluster.shutDownThisNode
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

  private def executeOrderMarkCommand(orderId: OrderId)(toEvent: OrderId => Checked[Option[OrderActorEvent]]) =
    _controllerState.idToOrder.checked(orderId) match {
      case Left(problem) =>
        Future.successful(Left(problem))

      case Right(order) =>
        toEvent(order.id) match {
          case Left(problem) =>
            Future.successful(Left(problem))
          case Right(None) =>
            Future.successful(Right(ControllerCommand.Response.Accepted))
          case Right(Some(event)) =>
            persist(orderId <-: event) { (stamped, updatedState) =>  // Event may be inserted between events coming from Agent
              handleOrderEvent(stamped, updatedState)
              Right(ControllerCommand.Response.Accepted)
            }
        }
    }

  private def applyRepoEvents(events: Seq[RepoEvent]): Checked[SyncIO[Future[Completed]]] = {
    def updateItems(diff: IntenvoryItems.Diff[TypedPath, InventoryItem]): Seq[Checked[SyncIO[Unit]]] =
      updateAgents(diff.select[AgentRefPath, AgentRef])

    def updateAgents(diff: IntenvoryItems.Diff[AgentRefPath, AgentRef]): Seq[Checked[SyncIO[Unit]]] =
      deletionNotSupported(diff) :+
        Right(SyncIO {
          for (agentRef <- diff.added) {
            val entry = registerAgent(agentRef, agentRunId = None, eventId = EventId.BeforeFirst)
            entry.actor ! AgentDriver.Input.StartFetchingEvents
          }
          for (agentRef <- diff.updated) {
            agentRegister.update(agentRef)
            agentRegister(agentRef.path).reconnect()
          }
        })

    def deletionNotSupported[P <: TypedPath, A <: InventoryItem](diff: IntenvoryItems.Diff[P, A])
      (implicit A: InventoryItem.Companion[A]): Seq[Left[Problem, Nothing]] =
      diff.deleted.map(o => Left(Problem.pure(s"Deletion of ${A.name} configuration objects is not supported: $o")))

    for {
      newVersionRepo <- _controllerState.repo.applyEvent(events.head) // May return DuplicateVersionProblem
      versionId = newVersionRepo.versionId
      changes <- events.tail.toVector.traverse(event => toRepoChange(event.asInstanceOf[ItemEvent]/*???*/))
      checkedSideEffects = updateItems(IntenvoryItems.Diff.fromRepoChanges(changes) withVersionId versionId)
      foldedSideEffects <- checkedSideEffects.toVector.sequence.map(_.fold(SyncIO.unit)(_ >> _))  // One problem invalidates all side effects
    } yield
      SyncIO {
        persistTransaction(events.map(KeyedEvent(_))) { (_, updatedState) =>
          _controllerState = updatedState
          events foreach logRepoEvent
          foldedSideEffects.unsafeRunSync()
          Completed
        }
      }
  }

  private def toRepoChange(event: ItemEvent): Checked[RepoChange] =
    event match {
      case ItemAdded(signed)   => _controllerState.repo.verify(signed) map RepoChange.Added
      case ItemChanged(signed) => _controllerState.repo.verify(signed) map RepoChange.Updated
      case ItemDeleted(path)   => Right(RepoChange.Deleted(path))
    }

  private def logRepoEvent(event: RepoEvent): Unit =
    event match {
      case VersionAdded(version) => logger.trace(s"Version '${version.string}' added")
      case o: ItemAdded          => logger.trace(s"${o.path} } added")
      case o: ItemChanged        => logger.trace(s"${o.path} } changed")
      case ItemDeleted(path)     => logger.trace(s"$path deleted")
    }

  private def registerAgent(agent: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId): AgentEntry = {
    val actor = watch(actorOf(
      AgentDriver.props(agent.path, agent.uri, agentRunId, eventId = eventId, agentDriverConfiguration, controllerConfiguration,
        journalActor = journalActor),
      encodeAsActorName("Agent-" + agent.path.withoutStartingSlash)))
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
            persist/*Async?*/(freshOrder.toOrderAdded(workflow.id.versionId)) { (stamped, updatedState) =>
              handleOrderEvent(stamped, updatedState)
              Right(true)
            }
            .flatMap(o => testAddOrderDelay.runToFuture.map(_ => o))  // test only
        }
    }

  private def addOrders(freshOrders: Seq[FreshOrder]): Future[Checked[Completed]] =
    freshOrders.checkUniqueness(_.id) match {
      case Left(problem) => Future.successful(Left(problem))
      case _ => freshOrders.toVector
        .filterNot(o => _controllerState.idToOrder contains o.id)  // Ignore known orders
        .traverse(o => _controllerState.repo.pathTo[Workflow](o.workflowPath).map(o.->))
        .traverse { ordersAndWorkflows =>
          val events = for ((order, workflow) <- ordersAndWorkflows) yield
            order.id <-: OrderAdded(workflow.id/*reuse*/, order.scheduledFor, order.arguments)
          persistMultiple(events) { (stampedEvents, updatedState) =>
            for (o <- stampedEvents) handleOrderEvent(o, updatedState)
            Completed
          }
        }
    }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]], updatedState: ControllerState): Unit = {
    val updatedOrderIds = handleOrderEventOnly(stamped)
    _controllerState = updatedState
    updatedOrderIds foreach proceedWithOrder
  }

  private def handleOrderEvents(stampedEvents: Seq[Stamped[KeyedEvent[OrderEvent]]], updatedState: ControllerState): Unit = {
    for (stamped <- stampedEvents) yield {
      val updatedOrderIds = handleOrderEventOnly(stamped)
      _controllerState = _controllerState.applyStampedEvents(stamped :: Nil).orThrow
      updatedOrderIds foreach proceedWithOrder
    }
    if (controllerConfiguration.journalConf.slowCheckState) {
      assertThat(_controllerState == updatedState)
    }
    // Reduce duplicate allocations
    _controllerState = updatedState
  }

  private def handleOrderEventOnly(stamped: Stamped[KeyedEvent[OrderEvent]]): Seq[OrderId] = {
    val KeyedEvent(orderId, event) = stamped.value
    event match {
      case _: OrderAdded =>
        orderId :: Nil

      case _ =>
        _controllerState.idToOrder.get(orderId) match {
          case None =>
            logger.error(s"Unknown OrderId in event ${orderId <-: event}")
            Nil

          case Some(order) =>
            val checkedFollowUps = orderEventHandler(_controllerState).handleEvent(orderId <-: event)
            val dependentOrderIds = mutable.Buffer[OrderId]()
            for (followUps <- checkedFollowUps onProblem (p => logger.error(p))) {  // TODO OrderBroken on error?
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

  private def proceedWithOrder(orderId: OrderId): Unit =
    if (!shuttingDown && switchover.isEmpty) {
      for (order <- _controllerState.idToOrder.get(orderId)) {
        for (mark <- order.mark collect { case o: OrderMark => o }) {
          if ((order.isAttaching || order.isAttached) && !orderMarkTransferredToAgent(order.id).contains(mark)) {
            // On Recovery, MarkOrder is sent again, because orderEntry.agentOrderMark is lost
            for ((_, _, agentEntry) <- checkedWorkflowJobAndAgentEntry(order).onProblem(p => logger.error(p))) {  // TODO OrderBroken on error?
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
    }

  private def proceedWithOrderOnController(order: Order[Order.State]): Unit = {
    order.state match {
      case _: Order.IsFreshOrReady if order.isProcessable =>
        val freshOrReady = order.castState[Order.IsFreshOrReady]
        instruction(order.workflowPosition) match {
          case _: Execute => tryAttachOrderToAgent(freshOrReady)
          case _ =>
        }

      case _: Order.Offering =>
        for (awaitingOrderId <- orderEventHandler(_controllerState).offeredToAwaitingOrder(order.id);
             awaitingOrder <- _controllerState.idToOrder.checked(awaitingOrderId).onProblem(p => logger.warn(p.toString));
             _ <- awaitingOrder.checkedState[Order.Awaiting].onProblem(p => logger.error(p.toString)))  // TODO OrderBroken on error?
        {
          proceedWithOrderOnController(awaitingOrder)
        }

      case _ =>
    }

    // When recovering, proceedWithOrderOnController may emit the same event multiple times,
    // for example OrderJoined for each parent and child order.
    // These events are collected and with actor message Internal.AfterProceedEventsAdded reduced to one.
    for (keyedEvent <- orderEventSource(_controllerState).nextEvent(order.id)) {
      keyedEvent match {
        case KeyedEvent(orderId, OrderBroken(problem)) =>
          logger.error(s"Order '${orderId.string}' is broken: $problem")
        case _ =>
      }
      afterProceedEvents.persistAndHandleLater(keyedEvent)
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.IsFreshOrReady]): Unit =
    for ((signedWorkflow, job, agentEntry) <- checkedWorkflowJobAndAgentEntry(order).onProblem(p => logger.error(p))) {  // TODO OrderBroken on error?
      if (order.isDetached && order.mark.isEmpty)
        persist(order.id <-: OrderAttachable(agentEntry.agentRefPath))(handleOrderEvent)
      else if (order.isAttaching) {
        agentEntry.actor ! AgentDriver.Input.AttachOrder(order, agentEntry.agentRefPath, signedWorkflow)  // OutOfMemoryError when Agent is unreachable !!!
      }
    }

  private def checkedWorkflowJobAndAgentEntry(order: Order[Order.State]): Checked[(Signed[Workflow], WorkflowJob, AgentEntry)] =
    for {
      signedWorkflow <- _controllerState.repo.idToSigned[Workflow](order.workflowId)
      job <- signedWorkflow.value.checkedWorkflowJob(order.position)
      agentEntry <- agentRegister.checked(job.agentRefPath)
    } yield (signedWorkflow, job, agentEntry)

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    _controllerState.idToOrder.checked(orderId)
      .flatMap(_.detaching)
      .onProblem(p => logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
      .foreach { agentRefPath =>
        agentRegister.get(agentRefPath) match {
          case None => logger.error(s"detachOrderFromAgent '$orderId': Unknown $agentRefPath")
          case Some(a) => a.actor ! AgentDriver.Input.DetachOrder(orderId)
        }
      }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    _controllerState.repo.idTo[Workflow](workflowPosition.workflowId).orThrow
      .instruction(workflowPosition.position)

  private def clusterSwitchOver(restart: Boolean)
  : Future[Checked[ControllerCommand.Response.Accepted.type]] =
    if (switchover.isDefined)
      Future.successful(Left(Problem("Already switching over")))
    else {
      val so = new Switchover(restart = restart)
      switchover = Some(so)
      so.start()
        .materialize.flatTap {
          case Success(Right(_)) => Task.unit  // this.switchover is left postStop
          case _ => Task {
            switchover = None  // Asynchronous!
          }
        }.dematerialize
        .guaranteeCase { exitCase =>
          Task {
            logger.debug(s"Switchover => $exitCase")
            so.close()
          }
        }
        .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
        .runToFuture
    }

  private def orderEventSource(controllerState: ControllerState) =
    new OrderEventSource(
      controllerState.repo.idTo[Workflow],
      controllerState.idToOrder.checked,
      isAgent = false)

  private def orderEventHandler(controllerState: ControllerState) =
    new OrderEventHandler(
      controllerState.repo.idTo[Workflow],
      controllerState.idToOrder.checked)

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
    final case class VerifiedUpdateRepoCmd(verifiedUpdateRepo: VerifiedUpdateRepo) extends Command
    final case class AddOrder(order: FreshOrder) extends Command
    final case class AddOrders(order: Seq[FreshOrder]) extends Command
  }

  sealed trait Reponse
  object Response {
    final case class ForAddOrder(created: Checked[Boolean])
  }

  private object Internal {
    final case class OtherClusterNodeActivationInhibited(recovered: Try[Recovered[ControllerState]])
    final case class ClusterModuleTerminatedUnexpectedly(tried: Try[Checked[Completed]]) extends DeadLetterSuppression
    final case class Ready(outcome: Checked[Completed])
    case object AfterProceedEventsAdded
    case object StillShuttingDown extends DeadLetterSuppression
    final case class ShutDown(shutdown: ControllerCommand.ShutDown)
  }

  private implicit final class RichIdToOrder(private val idToOrder: Map[OrderId, Order[Order.State]]) extends AnyVal {
    def checked(orderId: OrderId) = idToOrder.get(orderId).toChecked(UnknownOrderProblem(orderId))
  }

  private class AgentRegister extends ActorRegister[AgentRefPath, AgentEntry](_.actor) {
    override def insert(kv: (AgentRefPath, AgentEntry)) = super.insert(kv)
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
    def agentRefPath = agentRef.path

    def reconnect()(implicit sender: ActorRef): Unit =
      actor ! AgentDriver.Input.ChangeUri(uri = agentRef.uri)
  }

  private class OrderEntry
  {
    var agentOrderMark = none[OrderMark]
  }

  object ControllerReadyTestIncident
}
