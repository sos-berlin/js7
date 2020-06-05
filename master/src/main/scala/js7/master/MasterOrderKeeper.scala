package js7.master

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import cats.effect.SyncIO
import cats.instances.either._
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import js7.agent.data.event.AgentMasterEvent
import js7.base.crypt.{SignatureVerifier, Signed}
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable}
import js7.base.utils.ScalazStyle._
import js7.base.utils.SetOnce
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.akkautils.Akkas.encodeAsActorName
import js7.common.akkautils.SupervisorStrategies
import js7.common.configutils.Configs.ConvertibleConfig
import js7.common.scalautil.Logger
import js7.common.scalautil.Logger.ops._
import js7.core.command.CommandMeta
import js7.core.common.ActorRegister
import js7.core.event.journal.recover.{JournalRecoverer, Recovered}
import js7.core.event.journal.{JournalActor, MainJournalingActor}
import js7.core.filebased.FileBaseds
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.{AgentRef, AgentRefPath, AgentRunId}
import js7.data.cluster.ClusterState
import js7.data.crypt.FileBasedVerifier
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventId, JournalHeader, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.execution.workflow.OrderProcessor
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import js7.data.filebased.{FileBased, RepoEvent, TypedPath}
import js7.data.master.MasterFileBaseds
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderBroken, OrderCancellationMarked, OrderCoreEvent, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow}
import js7.master.MasterOrderKeeper._
import js7.master.agent.{AgentDriver, AgentDriverConfiguration}
import js7.master.cluster.Cluster
import js7.master.configuration.MasterConfiguration
import js7.master.data.agent.AgentEventIdEvent
import js7.master.data.events.MasterAgentEvent.AgentReady
import js7.master.data.events.MasterEvent
import js7.master.data.events.MasterEvent.{MasterShutDown, MasterTestEvent}
import js7.master.data.{MasterCommand, MasterState}
import js7.master.problems.MasterIsNotYetReadyProblem
import js7.master.repo.RepoCommandExecutor
import java.time.ZoneId
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
final class MasterOrderKeeper(
  stopped: Promise[MasterTermination],
  protected val journalActor: ActorRef @@ JournalActor.type,
  cluster: Cluster,
  masterConfiguration: MasterConfiguration,
  signatureVerifier: SignatureVerifier,
  testEventPublisher: EventPublisher[Any])
  (implicit protected val scheduler: Scheduler)
extends Stash
with MainJournalingActor[MasterState, Event]
{
  import context.{actorOf, watch}
  import masterConfiguration.config

  override val supervisorStrategy = SupervisorStrategies.escalate
  protected def journalConf = masterConfiguration.journalConf

  private val agentDriverConfiguration = AgentDriverConfiguration.fromConfig(config, masterConfiguration.journalConf).orThrow
  private var masterState: MasterState = MasterState.Undefined
  private val repoCommandExecutor = new RepoCommandExecutor(new FileBasedVerifier(signatureVerifier, MasterFileBaseds.jsonCodec))
  private val agentRegister = new AgentRegister
  private object orderRegister extends mutable.HashMap[OrderId, OrderEntry] {
    def checked(orderId: OrderId) = get(orderId).toChecked(UnknownOrderProblem(orderId))
  }
  private val idToOrder = orderRegister mapPartialFunction (_.order)
  private var orderProcessor = new OrderProcessor(PartialFunction.empty, idToOrder)
  private val recoveredJournalHeader = SetOnce[JournalHeader]
  private val suppressOrderIdCheckFor = config.optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  private val testAddOrderDelay = config.optionAs[FiniteDuration]("js7.TEST-ONLY.add-order-delay").fold(Task.unit)(Task.sleep)

  private def repo = masterState.repo

  private object shutdown {
    val since = SetOnce[Deadline]
    private val shutDown = SetOnce[MasterCommand.ShutDown]
    private val stillShuttingDownCancelable = SerialCancelable()
    private var terminatingAgentDrivers = false
    private var takingSnapshot = false
    private var snapshotTaken = false
    private var terminatingJournal = false

    def shuttingDown = since.isDefined

    def restart = shutDown.fold(false)(_.restart)

    def start(shutDown: MasterCommand.ShutDown): Unit =
      if (!shuttingDown) {
        since := now
        this.shutDown := shutDown
        stillShuttingDownCancelable := scheduler.scheduleAtFixedRates(masterConfiguration.journalConf.ackWarnDurations/*?*/) {
          self ! Internal.StillShuttingDown
        }
        journalActor ! JournalActor.Input.TakeSnapshot  // Take snapshot before OrderActors are stopped
        continue()
      }

    def close() =
      stillShuttingDownCancelable.cancel()

    def onStillShuttingDown() =
      logger.info(s"Still shutting down, waiting for ${agentRegister.runningActorCount} AgentDrivers" +
        (if (!snapshotTaken) " and the snapshot" else ""))

    def onSnapshotTaken(): Unit =
      if (shuttingDown) {
        snapshotTaken = true
        continue()
      }

    def continue() =
      if (shuttingDown) {
        logger.trace(s"shutdown.continue: ${agentRegister.runningActorCount} AgentDrivers${if (snapshotTaken) ", snapshot taken" else ""}")
        if (!terminatingAgentDrivers) {
          terminatingAgentDrivers = true
          agentRegister.values foreach {
            _.actor ! AgentDriver.Input.Terminate
          }
        }
        if (agentRegister.runningActorCount == 0 && !takingSnapshot) {
          takingSnapshot = true
          journalActor ! JournalActor.Input.TakeSnapshot
        }
        if (snapshotTaken && !terminatingJournal) {
          // The event forces the cluster to acknowledge this event and the snapshot taken
          terminatingJournal = true
          persistKeyedEventTask(NoKey <-: MasterShutDown())((_, _) => Completed)
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
  import shutdown.shuttingDown

  @volatile
  private var switchover: Option[Switchover] = None

  private final class Switchover(val restart: Boolean) {
    // 1) Emit SwitchedOver event
    // 2) Terminate JournalActor
    // 3) Stop MasterOrderKeeper includinge AgentDriver's
    // Do not terminate AgentDrivers properly because we do not want any events.

    private val stillSwitchingOverSchedule = scheduler.scheduleAtFixedRates(masterConfiguration.journalConf.ackWarnDurations) {
      logger.debug("Still switching over to the other cluster node")
    }

    def start(): Task[Checked[Completed]] =
      cluster.switchOver   // Will terminate `cluster`, letting MasterOrderKeeper terminate
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
      persistMultiple(events.distinct) { (stampedEvents, updatedState) =>
        masterState = updatedState
        stampedEvents foreach handleOrderEvent
        checkForEqualOrdersState()
        //checkForEqualOrdersState(stampedEvents.map(_.value.key).distinct)
      }
      events.clear()
    }
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
        if (switchover.exists(_.restart)) MasterTermination.Restart
        else MasterTermination.Terminate(restart = shutdown.restart))
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

  private def assertActiveClusterState(recovered: Recovered[MasterState]): Unit =
    for (clusterState <- recovered.recoveredState.map(_.clusterState)) {
      import masterConfiguration.clusterConf.ownId
      if (clusterState != ClusterState.Empty && !clusterState.isNonEmptyActive(ownId))
        throw new IllegalStateException(
          s"Master has recovered from Journal but is not the active node in ClusterState: id=$ownId, failedOver=$clusterState")
    }

  private def recover(recovered: Recovered[MasterState]): Unit = {
    for (masterState <- recovered.recoveredState) {
      if (masterState.masterMetaState.masterId != masterConfiguration.masterId)
        throw Problem(s"Recovered masterId='${masterState.masterMetaState.masterId}' differs from configured masterId='${masterConfiguration.masterId}'")
          .throwable
      this.masterState = masterState
      //masterMetaState = masterState.masterMetaState.copy(totalRunningTime = recovered.totalRunningTime)
      updateRepo()
      for (agentRef <- repo.currentFileBaseds collect { case o: AgentRef => o }) {
        val agentSnapshot = masterState.pathToAgentSnapshot.checked(agentRef.path).orThrow
        val e = registerAgent(agentRef, agentSnapshot.agentRunId, eventId = agentSnapshot.eventId)
        // Send an extra RegisterMe here, to be sure JournalActor has registered the AgentDriver when a snapshot is taken
        // TODO Fix fundamentally the race condition with JournalActor.Input.RegisterMe
        journalActor.tell(JournalActor.Input.RegisterMe, e.actor)
      }

      for (order <- masterState.idToOrder.values) {
        orderRegister.insert(order.id -> new OrderEntry(order))
      }
      persistedEventId = masterState.eventId
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
      case Command.Execute(_: MasterCommand.ShutDown, _) =>
        stash()

      case Command.Execute(cmd, _) =>
        logger.warn(s"$MasterIsNotYetReadyProblem: $cmd")
        sender ! Left(MasterIsNotYetReadyProblem)

      case cmd: Command =>
        logger.warn(s"$MasterIsNotYetReadyProblem: $cmd")
        sender ! Status.Failure(MasterIsNotYetReadyProblem.throwable)

      case _ => stash()
    }

  private def journalIsStarting: Receive = {
    case JournalRecoverer.Output.JournalIsReady(journalHeader) =>
      recoveredJournalHeader := journalHeader
      become("becomingReady")(becomingReady)  // `become` must be called early, before any persist!

      persistMultiple(
        (!masterState.masterMetaState.isDefined ?
          (NoKey <-: MasterEvent.MasterInitialized(masterConfiguration.masterId, journalHeader.startedAt))
        ) ++ Some(NoKey <-: MasterEvent.MasterReady(ZoneId.systemDefault.getId, totalRunningTime = journalHeader.totalRunningTime))
      ) { (_, updatedMasterState) =>
        masterState = updatedMasterState
        cluster.afterJounalingStarted
          .materializeIntoChecked
          .runToFuture
          .map(Internal.Ready.apply)
          .pipeTo(self)
      }

      // Proceed order before starting AgentDrivers, so AgentDrivers may match recovered OrderIds with Agent's OrderIds
      for (order <- orderRegister.values.toVector/*copy*/) {  // Any ordering when continuing orders???
        proceedWithOrder(order)  // May persist events!
      }
      afterProceedEvents.persistThenHandleEvents()  // Persist and handle before Internal.Ready
      if (persistedEventId > EventId.BeforeFirst) {  // Recovered?
        logger.info(s"${orderRegister.size} Orders, ${repo.typedCount[Workflow]} Workflows and ${repo.typedCount[AgentRef]} AgentRefs recovered")
      }

      // Start fetching events from Agents after AttachOrder has been sent to AgentDrivers.
      // This is to handle race-condition: An Agent may have already completed an order.
      // So send AttachOrder before DetachOrder.
      // The Agent will ignore the duplicate AttachOrder if it arrives before DetachOrder.
      agentRegister.values foreach {
        _.actor ! AgentDriver.Input.StartFetchingEvents
      }

    case Command.Execute(_: MasterCommand.ShutDown, _) =>
      stash()

    case Command.Execute(cmd, _) =>
      logger.warn(s"$MasterIsNotYetReadyProblem: $cmd")
      sender ! Left(MasterIsNotYetReadyProblem)

    case cmd: Command =>
      logger.warn(s"$MasterIsNotYetReadyProblem: $cmd")
      sender ! Status.Failure(MasterIsNotYetReadyProblem.throwable)

    case _ => stash()
  }

  private def becomingReady: Receive = {
    case Internal.Ready(Left(problem)) =>
      logger.error(s"Appointment of configured cluster backup-node failed: $problem")
      throw problem.throwable.appendCurrentStackTrace

    case Internal.Ready(Right(Completed)) =>
      logger.info("Ready")
      testEventPublisher.publish(MasterReadyTestIncident)
      cluster.onTerminatedUnexpectedly.runToFuture onComplete { tried =>
        self ! Internal.ClusterModuleTerminatedUnexpectedly(tried)
      }
      become("Ready")(ready orElse handleExceptionalMessage)
      unstashAll()

    case _ =>
      // stash Command too, after MasterReady event and cluster node has been initialized (see above)
      stash()
  }

  private def ready: Receive = {
    case Internal.AfterProceedEventsAdded =>
      afterProceedEvents.persistThenHandleEvents()

    case Command.Execute(command, meta) =>
      val sender = this.sender()
      if (shuttingDown)
        sender ! Status.Failure(MasterIsShuttingDownProblem.throwable)
      else if (switchover.isDefined)
        sender ! Status.Failure(MasterIsSwitchingOverProblem.throwable)
      else
        executeMasterCommand(command, meta) onComplete {
          case Failure(t) => sender ! Status.Failure(t)
          case Success(response) => sender ! response
        }

    case Command.AddOrder(order) =>
      if (shuttingDown)
        sender() ! Status.Failure(MasterIsShuttingDownProblem.throwable)
      else if (switchover.isDefined)
        sender() ! Status.Failure(MasterIsSwitchingOverProblem.throwable)
      else
        addOrder(order) map Response.ForAddOrder.apply pipeTo sender()

    case Command.AddOrders(orders) =>
      if (shuttingDown)
        sender() ! Status.Failure(MasterIsShuttingDownProblem.throwable)
      else if (switchover.isDefined)
        sender() ! Status.Failure(MasterIsSwitchingOverProblem.throwable)
      else
        addOrders(orders).pipeTo(sender())

    case AgentDriver.Output.EventsFromAgent(stampeds, completedPromise) =>
      val agentEntry = agentRegister(sender())
      import agentEntry.agentRefPath
      var lastAgentEventId: Option[EventId] = None
      var masterStamped: Seq[Timestamped[Event]] =
        stampeds.view.flatMap {
          case Stamped(agentEventId, timestamp, keyedEvent) =>
            // TODO Event vor dem Speichern mit Order.applyEvent ausprobieren! Bei Fehler ignorieren?
            lastAgentEventId = Some(agentEventId)
            keyedEvent match {
              case KeyedEvent(_, _: OrderCancellationMarked) =>  // We (the Master) emit our own OrderCancellationMarked
                None

              case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                val ownEvent = event match {
                  case _: OrderEvent.OrderAttached => OrderTransferredToAgent(agentRefPath) // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                  case _ => event
                }
                Some(Timestamped(orderId <-: ownEvent, Some(timestamp)))

              case KeyedEvent(_: NoKey, AgentMasterEvent.AgentReadyForMaster(timezone, _)) =>
                Some(Timestamped(agentEntry.agentRefPath <-: AgentReady(timezone), Some(timestamp)))

              case _ =>
                logger.warn(s"Unknown event received from ${agentEntry.agentRefPath}: $keyedEvent")
                None
            }
        }.toVector
      masterStamped ++= lastAgentEventId.map(agentEventId => Timestamped(agentRefPath <-: AgentEventIdEvent(agentEventId)))

      completedPromise.completeWith(
        persistTransactionTimestamped(masterStamped, async = true, alreadyDelayed = agentDriverConfiguration.eventBufferDelay) {
          (stampedEvents, updatedState) =>
            masterState = updatedState
            // Inhibit OrderAdded, OrderFinished, OrderJoined(?), OrderAttachable and others ???
            //  Agent does not send these events, but just in case.
            stampedEvents.map(_.value)
              .foreach {
                case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                  handleOrderEvent(orderId, event)

                case _ =>
              }
            checkForEqualOrdersState()
            Completed
        })

    case AgentDriver.Output.OrdersDetached(orderIds) =>
      val unknown = orderIds -- orderRegister.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.DetachOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      persistMultipleAsync(orderIds -- unknown map (_ <-: OrderTransferredToMaster)) { (stampedEvents, updatedState) =>
        masterState = updatedState
        stampedEvents foreach handleOrderEvent
        checkForEqualOrdersState()
      }

    case AgentDriver.Output.OrdersCancellationMarked(orderIds) =>
      val unknown = orderIds -- orderRegister.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.CancelOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      for (orderId <- orderIds) {
        orderRegister(orderId).cancelationMarkedOnAgent = true
      }

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case Internal.ShutDown(shutDown) =>
      shutdown.start(shutDown)

    case Internal.StillShuttingDown =>
      shutdown.onStillShuttingDown()

    case Terminated(a) if agentRegister contains a =>
      agentRegister(a).actorTerminated = true
      shutdown.continue()
  }

  // JournalActor's termination must be handled in any `become`-state and must lead to MasterOrderKeeper's termination
  override def journaling = handleExceptionalMessage orElse super.journaling

  private def handleExceptionalMessage: Receive = {
    case Terminated(`journalActor`) =>
      if (!shuttingDown && switchover.isEmpty) logger.error("JournalActor terminated")
      context.stop(self)

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

  private def executeMasterCommand(command: MasterCommand, commandMeta: CommandMeta): Future[Checked[MasterCommand.Response]] =
    command match {
      case MasterCommand.AddOrder(order) =>
        if (shuttingDown)
          Future.successful(Left(MasterIsShuttingDownProblem))
        else if (switchover.isDefined)
          Future.successful(Left(MasterIsSwitchingOverProblem))
        else
          addOrder(order)
            .map(_.map(added => MasterCommand.AddOrder.Response(ignoredBecauseDuplicate = !added)))
            .pipeTo(sender())

      case MasterCommand.CancelOrder(orderId, mode) =>
        orderRegister.checked(orderId) map (_.order) match {
          case Left(problem) =>
            Future.successful(Left(problem))

          case Right(order) =>
            orderProcessor.cancel(order.id, mode, isAgent = false) match {
              case Left(problem) =>
                Future.successful(Left(problem))
              case Right(None) =>
                Future.successful(Right(MasterCommand.Response.Accepted))
              case Right(Some(event)) =>
                persist(orderId <-: event) { (stamped, updatedState) =>  // Event may be inserted between events coming from Agent
                  masterState = updatedState
                  handleOrderEvent(stamped)
                  checkForEqualOrdersState()
                  Right(MasterCommand.Response.Accepted)
                }
            }
        }

      case MasterCommand.ReleaseEvents(untilEventId) =>
        val userId = commandMeta.user.id
        if (!masterConfiguration.journalConf.releaseEventsUserIds.contains(userId))
          Future(Left(UserIsNotEnabledToReleaseEventsProblem))
        else {
          val current = masterState.journalState.userIdToReleasedEventId.getOrElse(userId, EventId.BeforeFirst)
          if (untilEventId < current)
            Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = untilEventId, currentUntilEventId = current)))
          else
            persist(JournalEventsReleased(userId, untilEventId)) { (_, updatedState) =>
              masterState = updatedState
              Right(MasterCommand.Response.Accepted)
            }
        }

      case cmd: MasterCommand.ReplaceRepo =>
        intelliJuseImport(catsStdInstancesForFuture)  // For traverse
        repoCommandExecutor.replaceRepoCommandToEvents(repo, cmd, commandMeta)
          .flatMap(readConfiguration)
          .traverse((_: SyncIO[Future[Completed]])
            .unsafeRunSync()  // Persist events!
            .map(_ => MasterCommand.Response.Accepted))

      case cmd: MasterCommand.UpdateRepo =>
        repoCommandExecutor.updateRepoCommandToEvents(repo, cmd, commandMeta)
          .flatMap(readConfiguration)
          .traverse((_: SyncIO[Future[Completed]])
            .unsafeRunSync()  // Persist events!
            .map(_ => MasterCommand.Response.Accepted))

      case MasterCommand.NoOperation =>
        // NoOperation completes only after MasterOrderKeeper has become ready (can be used to await readiness)
        Future.successful(Right(MasterCommand.Response.Accepted))

      case _: MasterCommand.EmergencyStop | _: MasterCommand.Batch =>       // For completeness. RunningMaster has handled the command already
        Future.successful(Left(Problem.pure("THIS SHOULD NOT HAPPEN")))  // Never called

      case MasterCommand.TakeSnapshot =>
        import masterConfiguration.akkaAskTimeout  // We need several seconds or even minutes
        intelliJuseImport(akkaAskTimeout)
        (journalActor ? JournalActor.Input.TakeSnapshot)
          .mapTo[JournalActor.Output.SnapshotTaken.type]
          .map(_ => Right(MasterCommand.Response.Accepted))

      case cmd @ MasterCommand.ClusterSwitchOver =>
        clusterSwitchOver(restart = true)

      case shutDown: MasterCommand.ShutDown =>
        shutDown.clusterAction match {
          case Some(MasterCommand.ShutDown.ClusterAction.Switchover) =>
            clusterSwitchOver(restart = shutDown.restart)

          case Some(MasterCommand.ShutDown.ClusterAction.Failover) =>
            // TODO ClusterState.Coupled !
            shutdown.start(shutDown)
            Future.successful(Right(MasterCommand.Response.Accepted))

          case None =>
            cluster.shutDownThisNode
              .flatTap {
                case Right(Completed) => Task { self ! Internal.ShutDown(shutDown) }
                case _ => Task.unit
              }
              .map(_.map((_: Completed) => MasterCommand.Response.Accepted))
              .runToFuture
        }

      case MasterCommand.EmitTestEvent =>
        persist(MasterTestEvent, async = true) { (_, updatedState) =>
          masterState = updatedState
          Right(MasterCommand.Response.Accepted)
        }

      case _ =>
        // Handled by MasterCommandExecutor
        Future.failed(new NotImplementedError)
    }

  private def readConfiguration(events: Seq[RepoEvent]): Checked[SyncIO[Future[Completed]]] = {
    def updateFileBaseds(diff: FileBaseds.Diff[TypedPath, FileBased]): Seq[Checked[SyncIO[Unit]]] =
      updateAgents(diff.select[AgentRefPath, AgentRef])

    def updateAgents(diff: FileBaseds.Diff[AgentRefPath, AgentRef]): Seq[Checked[SyncIO[Unit]]] =
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

    def deletionNotSupported[P <: TypedPath, A <: FileBased](diff: FileBaseds.Diff[P, A])
      (implicit A: FileBased.Companion[A]): Seq[Left[Problem, Nothing]] =
      diff.deleted.map(o => Left(Problem.pure(s"Deletion of ${A.name} configuration objects is not supported: $o")))

    //def changeNotSupported[P <: TypedPath, A <: FileBased](diff: FileBaseds.Diff[P, A]): Seq[Left[Problem]] =
    //  diff.updated.map(o => Left(Problem(s"Change of these configuration objects is not supported: ${o.path}")))

    for {
      changedRepo <- repo.applyEvents(events)  // May return DuplicateVersionProblem
      realChanges = FileBaseds.diffFileBaseds(changedRepo.currentFileBaseds, repo.currentFileBaseds)  // should be equivalent to events
      checkedSideEffects = updateFileBaseds(FileBaseds.Diff.fromRepoChanges(realChanges) withVersionId changedRepo.versionId)
      foldedSideEffects <- checkedSideEffects.toVector.sequence map (_.fold(SyncIO.unit)(_ >> _))  // One problem invalidates all side effects
    } yield
      SyncIO {
        persistTransaction(events.map(KeyedEvent(_))) { (_, updatedState) =>
          masterState = updatedState
          updateRepo()
          events foreach logRepoEvent
          foldedSideEffects.unsafeRunSync()
          Completed
        }
      }
  }

  private def logRepoEvent(event: RepoEvent): Unit =
    event match {
      case VersionAdded(version)     => logger.info(s"Version '${version.string}' added")
      case FileBasedAdded(path, _)   => logger.info(s"Added $path")
      case FileBasedChanged(path, _) => logger.info(s"Changed $path")
      case FileBasedDeleted(path)    => logger.info(s"Deleted $path")
    }

  private def updateRepo(): Unit =
    orderProcessor = new OrderProcessor(repo.idTo[Workflow], idToOrder)

  private def registerAgent(agent: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId): AgentEntry = {
    val actor = watch(actorOf(
      AgentDriver.props(agent.path, agent.uri, agentRunId, eventId = eventId, agentDriverConfiguration, masterConfiguration,
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
    orderRegister.get(freshOrder.id) match {
      case Some(_) =>
        logger.debug(s"Discarding duplicate added Order: $freshOrder")
        Future.successful(Right(false))

      case None =>
        repo.pathTo[Workflow](freshOrder.workflowPath) match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(workflow) =>
            val order = freshOrder.toOrder(workflow.id.versionId)
            persist/*Async?*/(order.id <-: OrderAdded(workflow.id, order.state.scheduledFor, order.arguments)) { (stamped, updatedState) =>
              masterState = updatedState
              handleOrderEvent(stamped)
              checkForEqualOrdersState()
              Right(true)
            }
            .flatMap(o => testAddOrderDelay.runToFuture.map(_ => o))  // test only
        }
    }

  private def addOrders(freshOrders: Seq[FreshOrder]): Future[Checked[Completed]] =
    freshOrders.toVector
      .filterNot(o => orderRegister.contains(o.id))  // Ignore known orders
      .traverse(o => repo.pathTo[Workflow](o.workflowPath).map(o.->))
      .traverse { ordersAndWorkflows =>
        val events = for ((order, workflow) <- ordersAndWorkflows) yield
          order.id <-: OrderAdded(workflow.id/*reuse*/, order.scheduledFor, order.arguments)
        persistMultiple(events) { (stampedEvents, updatedState) =>
          masterState = updatedState
          for (o <- stampedEvents) handleOrderEvent(o)
          checkForEqualOrdersState()
          Completed
        }
      }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): Unit =
    handleOrderEvent(stamped.value.key, stamped.value.event)

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit = {
    event match {
      case event: OrderAdded =>
        registerOrderAndProceed(Order.fromOrderAdded(orderId, event))

      case _ =>
        orderRegister.get(orderId) match {
          case None =>
            logger.error(s"Unknown OrderId in event ${orderId <-: event}")

          case Some(orderEntry) =>
            val checkedFollowUps = orderProcessor.handleEvent(orderId <-: event)
            for (followUps <- checkedFollowUps onProblem (p => logger.error(p))) {  // TODO OrderBroken on error?
              followUps foreach {
                case _: FollowUp.Processed if orderEntry.order.isAttached =>

                case FollowUp.AddChild(childOrder) =>
                  registerOrderAndProceed(childOrder)

                case FollowUp.AddOffered(offeredOrder) =>
                  registerOrderAndProceed(offeredOrder)

                case FollowUp.Remove(removeOrderId) =>
                  orderRegister -= removeOrderId

                case unexpected =>
                  logger.error(s"Order '$orderId': Unexpected FollowUp $unexpected")
              }
            }
            orderEntry.update(event)
            if (orderRegister contains orderId) {  // orderEntry has not been deleted?
              proceedWithOrder(orderEntry)
            }
        }
    }
  }

  private def registerOrderAndProceed(order: Order[Order.State]): Unit = {
    val entry = new OrderEntry(order)
    orderRegister.insert(order.id -> entry)
    proceedWithOrder(entry)
  }

  private def proceedWithOrder(orderEntry: OrderEntry): Unit =
    if (!shuttingDown && switchover.isEmpty) {
      val order = orderEntry.order
      for (mode <- order.cancel) {
        if ((order.isAttaching || order.isAttached) && !orderEntry.cancelationMarkedOnAgent) {
          // On Recovery, CancelOrder is sent again, because orderEntry.cancelationMarkedOnAgent is lost
          for ((_, _, agentEntry) <- checkedWorkflowJobAndAgentEntry(order) onProblem (p => logger.error(p))) {  // TODO OrderBroken on error?
            agentEntry.actor ! AgentDriver.Input.CancelOrder(order.id, mode)
          }
        }
      }
      order.attachedState match {
        case None |
             Some(_: Order.Attaching) => proceedWithOrderOnMaster(orderEntry)
        case Some(_: Order.Attached)  =>
        case Some(_: Order.Detaching) => detachOrderFromAgent(order.id)
      }
    }

  private def proceedWithOrderOnMaster(orderEntry: OrderEntry): Unit = {
    val order = orderEntry.order
    order.state match {
      case _: Order.IsFreshOrReady =>
        val freshOrReady = order.castState[Order.IsFreshOrReady]
        instruction(order.workflowPosition) match {
          case _: Execute => tryAttachOrderToAgent(freshOrReady)
          case _ =>
        }

      case _: Order.Offering =>
        for (awaitingOrderId <- orderProcessor.offeredToAwaitingOrder(orderEntry.orderId);
             awaitingOrder <- orderRegister.checked(awaitingOrderId).onProblem(p => logger.warn(p.toString));
             _ <- awaitingOrder.order.checkedState[Order.Awaiting].onProblem(p => logger.error(p.toString)))  // TODO OrderBroken on error?
        {
          proceedWithOrderOnMaster(awaitingOrder)
        }

      case _ =>
    }

    // When recovering, proceedWithOrderOnMaster may emit the same event multiple times,
    // for example OrderJoined for each parent and child order.
    // These events are collected and with actor message Internal.AfterProceedEventsAdded reduced to one.
    for (keyedEvent <- orderProcessor.nextEvent(order.id)) {
      keyedEvent match {
        case KeyedEvent(orderId, OrderBroken(problem)) =>
          logger.error(s"Order ${orderId.string} is broken: $problem")
        case _ =>
      }
      afterProceedEvents.persistAndHandleLater(keyedEvent)
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.IsFreshOrReady]): Unit =
    for ((signedWorkflow, job, agentEntry) <- checkedWorkflowJobAndAgentEntry(order).onProblem(p => logger.error(p))) {  // TODO OrderBroken on error?
      if (order.isDetached && !orderProcessor.isOrderCancelable(order))
        persist(order.id <-: OrderAttachable(agentEntry.agentRefPath)) { (stamped, updatedState) =>
          masterState = updatedState
          handleOrderEvent(stamped)
          checkForEqualOrdersState()
        }
      else if (order.isAttaching) {
        agentEntry.actor ! AgentDriver.Input.AttachOrder(order, agentEntry.agentRefPath, signedWorkflow)  // OutOfMemoryError when Agent is unreachable !!!
      }
    }

  private def checkedWorkflowJobAndAgentEntry(order: Order[Order.State]): Checked[(Signed[Workflow], WorkflowJob, AgentEntry)] =
    for {
      signedWorkflow <- repo.idToSigned[Workflow](order.workflowId)
      job <- signedWorkflow.value.checkedWorkflowJob(order.position)
      agentEntry <- agentRegister.checked(job.agentRefPath)
    } yield (signedWorkflow, job, agentEntry)

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    orderRegister(orderId).order.detaching
      .onProblem(p => logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
      .foreach { agentRefPath =>
        agentRegister.get(agentRefPath) match {
          case None => logger.error(s"detachOrderFromAgent '$orderId': Unknown $agentRefPath")
          case Some(a) => a.actor ! AgentDriver.Input.DetachOrder(orderId)
        }
      }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    repo.idTo[Workflow](workflowPosition.workflowId).orThrow.instruction(workflowPosition.position)

  private def clusterSwitchOver(restart: Boolean)
  : Future[Checked[MasterCommand.Response.Accepted.type]] =
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
        .map(_.map((_: Completed) => MasterCommand.Response.Accepted))
        .runToFuture
    }

  private def checkForEqualOrdersState(): Unit =
    if (masterConfiguration.journalConf.slowCheckState) {
      assertThat(masterState.idToOrder.size == orderRegister.size)
      masterState.idToOrder.keysIterator foreach checkForEqualOrderState
    }

  private def checkForEqualOrderState(orderId: OrderId): Unit =
    assertThat(masterState.idToOrder.get(orderId) == orderRegister.get(orderId).map(_.order), orderId.toString)

  override def toString = "MasterOrderKeeper"
}

private[master] object MasterOrderKeeper
{
  private object MasterIsShuttingDownProblem extends Problem.ArgumentlessCoded
  private object MasterIsSwitchingOverProblem extends Problem.ArgumentlessCoded

  private val logger = Logger(getClass)

  object Input {
    final case class Start(recovered: Recovered[MasterState])
  }

  sealed trait Command
  object Command {
    final case class Execute(command: MasterCommand, meta: CommandMeta) extends Command
    final case class AddOrder(order: FreshOrder) extends Command
    final case class AddOrders(order: Seq[FreshOrder]) extends Command
  }

  sealed trait Reponse
  object Response {
    final case class ForAddOrder(created: Checked[Boolean])
  }

  private object Internal {
    final case class OtherClusterNodeActivationInhibited(recovered: Try[Recovered[MasterState]])
    final case class ClusterModuleTerminatedUnexpectedly(tried: Try[Checked[Completed]]) extends DeadLetterSuppression
    final case class Ready(outcome: Checked[Completed])
    case object AfterProceedEventsAdded
    case object StillShuttingDown extends DeadLetterSuppression
    final case class ShutDown(shutdown: MasterCommand.ShutDown)
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

  private class OrderEntry(private var _order: Order[Order.State])
  {
    def order = _order

    var cancelationMarkedOnAgent = false

    def orderId = order.id

    def update(event: OrderEvent): Unit =
      event match {
        case _: OrderStdWritten =>
        case event: OrderCoreEvent =>
          _order.update(event) match {
            case Left(problem) => logger.error(problem.toString)  // TODO Invalid event stored and ignored. Should we validate the event before persisting?
              // TODO Mark order as unusable (and try OrderBroken). No further actions on this order to avoid loop!
            case Right(o) => _order = o
          }
      }
  }

  object MasterReadyTestIncident
}
