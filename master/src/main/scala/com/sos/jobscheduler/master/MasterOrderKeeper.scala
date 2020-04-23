package com.sos.jobscheduler.master

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import cats.effect.SyncIO
import cats.instances.either._
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.eventbus.EventBus
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.monixutils.MonixBase.syntax._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable}
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.SetOnce
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.event.journal.recover.{JournalRecoverer, Recovered}
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.filebased.{FileBasedVerifier, FileBaseds, Repo}
import com.sos.jobscheduler.core.problems.{ReverseReleaseEventsProblem, UnknownOrderProblem}
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.crypt.Signed
import com.sos.jobscheduler.data.event.JournalEvent.JournalEventsReleased
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{<-:, Event, EventId, JournalState, JournaledState, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath}
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderBroken, OrderCancellationMarked, OrderCoreEvent, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.problems.UserIsNotEnabledToReleaseEventsProblem
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.master.MasterOrderKeeper._
import com.sos.jobscheduler.master.agent.{AgentDriver, AgentDriverConfiguration}
import com.sos.jobscheduler.master.cluster.Cluster
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.agent.{AgentEventIdEvent, AgentSnapshot}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentReady
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.master.data.events.MasterEvent.{MasterShutDown, MasterTestEvent}
import com.sos.jobscheduler.master.problems.MasterIsNotYetReadyProblem
import com.sos.jobscheduler.master.repo.RepoCommandExecutor
import java.time.ZoneId
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.SerialCancelable
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise, blocking}
import scala.util.control.NonFatal
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
  testEventBus: EventBus)
  (implicit scheduler: Scheduler)
extends Stash
with MainJournalingActor[MasterState, Event]
{
  import context.{actorOf, watch}
  import masterConfiguration.config

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val agentDriverConfiguration = AgentDriverConfiguration.fromConfig(config, masterConfiguration.journalConf).orThrow
  private var masterMetaState = MasterMetaState.Undefined
  private var repo = Repo(MasterFileBaseds.jsonCodec)
  private val repoCommandExecutor = new RepoCommandExecutor(new FileBasedVerifier(signatureVerifier, MasterFileBaseds.jsonCodec))
  private val agentRegister = new AgentRegister
  private object orderRegister extends mutable.HashMap[OrderId, OrderEntry] {
    def checked(orderId: OrderId) = get(orderId).toChecked(UnknownOrderProblem(orderId))
  }
  private val idToOrder = orderRegister mapPartialFunction (_.order)
  private var orderProcessor = new OrderProcessor(PartialFunction.empty, idToOrder)
  private var journalState = JournalState.empty
  private val recoveredJournalHeader = SetOnce[JournalHeader]
  private val suppressOrderIdCheckFor = config.optionAs[String]("jobscheduler.TEST-ONLY.suppress-order-id-check-for")
  private val testAddOrderDelay = config.optionAs[FiniteDuration]("jobscheduler.TEST-ONLY.add-order-delay").fold(Task.unit)(Task.sleep)

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
        stillShuttingDownCancelable := scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
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
                case Success(Completed) =>
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
    // 1) Issue SwitchedOver event
    // 2) Terminate JournalActor
    // 3) Stop MasterOrderKeeper includinge AgentDriver's
    // Do not terminate AgentDrivers properly because we do not want any events.

    private val stillSwitchingOverSchedule = scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
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
      // Eliminate duplicate events like OrderJoined, which may be issued by parent and child orders when recovering
      persistMultiple(events.distinct) { (stampedEvents, journaledState) =>
        stampedEvents foreach handleOrderEvent
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

  protected def snapshots = masterState.toSnapshotObservable.toListL.runToFuture

  private def masterState: MasterState = {
    // FIXME Blockiert, damit ClusterState vom selben Zeitpunkt ist. EventId des ClusterState kann abweichen!
    // TODO Einheitliches MasterState halten, also einheitliches JournaledStatePersistence
    logger.debug("masterState blocking read ...")
    val clusterState = blocking {
      cluster.currentClusterState
        .runSyncUnsafe(masterConfiguration.akkaAskTimeout.duration)
    }
    logger.debug("masterState blocking read okay")
    MasterState(
      persistedEventId,
      JournaledState.Standards(journalState, clusterState),
      masterMetaState,
      repo,
      pathToAgentSnapshot = agentRegister.values.map(entry => entry.agentRefPath -> entry.toSnapshot).toMap,
      orderRegister.view.mapValues(_.order).toMap)
  }

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
      masterMetaState = masterState.masterMetaState
      //masterMetaState = masterState.masterMetaState.copy(totalRunningTime = recovered.totalRunningTime)
      setRepo(masterState.repo)
      for (agentRef <- repo.currentFileBaseds collect { case o: AgentRef => o }) {
        val agentSnapshot = masterState.pathToAgentSnapshot.checked(agentRef.path).orThrow
        val e = registerAgent(agentRef, agentSnapshot.agentRunId, eventId = agentSnapshot.eventId)
        // Send an extra RegisterMe here, to be sure JournalActor has registered the AgentDriver when a snapshot is taken
        // TODO Fix fundamentally the race condition with JournalActor.Input.RegisterMe
        journalActor.tell(JournalActor.Input.RegisterMe, e.actor)
      }
      for (agentSnapshot <- masterState.pathToAgentSnapshot.values) {
        agentRegister(agentSnapshot.agentRefPath).lastAgentEventId = agentSnapshot.eventId
      }
      for (order <- masterState.idToOrder.values) {
        orderRegister.insert(order.id -> new OrderEntry(order))
      }
      journalState = masterState.journalState
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
        (!masterMetaState.isDefined ?
          (NoKey <-: MasterEvent.MasterInitialized(masterConfiguration.masterId, journalHeader.startedAt))
        ) ++ Some(NoKey <-: MasterEvent.MasterReady(ZoneId.systemDefault.getId, totalRunningTime = journalHeader.totalRunningTime))
      ) { (stampedEvents, journaledState) =>
        stampedEvents.map(_.value) foreach {
           case KeyedEvent(NoKey, MasterEvent.MasterInitialized(masterId, startedAt)) =>
             masterMetaState = masterMetaState.copy(masterId = masterId, startedAt = startedAt)
           case _ =>
         }
        testEventBus.publish(MasterReadyTestIncident)
        cluster.afterJounalingStarted
          .materializeIntoChecked
          .runToFuture
          .map(Internal.Ready.apply)
          .pipeTo(self)
      }

      for (order <- orderRegister.values.toVector/*copy*/) {  // Any ordering when continuing orders???
        proceedWithOrder(order)  // May persist events! May send AttachOrder to AgentDriver!
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

    case Command.GetRepo =>
      sender() ! Stamped(persistedEventId, repo)

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

    case Command.GetOrder(orderId) =>
      sender() ! (orderRegister.get(orderId) map { _.order })

    case Command.GetOrders =>
      sender() ! Stamped[Vector[Order[Order.State]]](persistedEventId, orderRegister.values.map(_.order).toVector)

    case Command.GetOrderCount =>
      sender() ! (orderRegister.size: Int)

    case Command.GetState =>
      sender() ! (
        try masterState
        catch { case NonFatal(t) =>  // Maybe Akka ask timeout
          sender() ! Status.Failure(t)
        })

    case AgentDriver.Output.RegisteredAtAgent(agentRunId) =>
      val agentEntry = agentRegister(sender())
      if (agentEntry.agentRunId.isEmpty) {
        agentEntry.agentRunId := agentRunId
      }

    case AgentDriver.Output.EventsFromAgent(stampeds, completedPromise) =>
      val agentEntry = agentRegister(sender())
      import agentEntry.agentRefPath
      var lastAgentEventId: Option[EventId] = None
      var masterStamped: Seq[Timestamped[Event]] = stampeds.flatMap {
        case stamped @ Stamped(agentEventId, timestamp, keyedEvent) =>
          if (agentEventId <= lastAgentEventId.getOrElse(agentEntry.lastAgentEventId)) {
            logger.debug(s"AgentDriver ${agentEntry.agentRefPath} has returned old (<= ${lastAgentEventId.getOrElse(agentEntry.lastAgentEventId)}) event: $stamped")
            None
          } else {
            // TODO Event vor dem Speichern mit Order.applyEvent ausprobieren! Bei Fehler ignorieren?
            lastAgentEventId = Some(agentEventId)
            keyedEvent match {
              case KeyedEvent(_, _: OrderCancellationMarked) =>  // We (the Master) issue our own OrderCancellationMarked
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
          }
      }
      masterStamped ++= lastAgentEventId.map(agentEventId => Timestamped(agentRefPath <-: AgentEventIdEvent(agentEventId)))

      persistTransactionTimestamped(masterStamped, async = true, alreadyDelayed = agentDriverConfiguration.eventBufferDelay) {
        (stampedEvents, journaledState) =>
          // Inhibit OrderAdded, OrderFinished, OrderJoined(?), OrderAttachable and others ???
          //  Agent does not send these events, but just in case.
          stampedEvents.map(_.value)
            .foreach {
              case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                handleOrderEvent(orderId, event)

              case KeyedEvent(_, AgentEventIdEvent(agentEventId)) =>
                assert(agentEntry.lastAgentEventId < agentEventId)
                agentEntry.lastAgentEventId = agentEventId

              case _ =>
            }
          completedPromise.success(Completed)
      }

    case AgentDriver.Output.OrdersDetached(orderIds) =>
      val unknown = orderIds -- orderRegister.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.DetachOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      persistMultipleAsync(orderIds -- unknown map (_ <-: OrderTransferredToMaster)) { (stampedEvents, journaledState) =>
        stampedEvents foreach handleOrderEvent
      }

    case AgentDriver.Output.OrdersCancelationMarked(orderIds) =>
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
      // Stacktrace is being debug-logged by Cluster
      logger.error(s"Cluster module terminated unexpectedly with $tried ")
      context.stop(self)
  }

  private def executeMasterCommand(command: MasterCommand, commandMeta: CommandMeta): Future[Checked[MasterCommand.Response]] =
    command match {
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
                persist(orderId <-: event) { (stamped, journaledState) =>  // Event may be inserted between events coming from Agent
                  handleOrderEvent(stamped)
                  Right(MasterCommand.Response.Accepted)
                }
            }
        }

      case MasterCommand.ReleaseEvents(untilEventId) =>
        val userId = commandMeta.user.id
        if (!masterConfiguration.journalConf.releaseEventsUserIds.contains(userId))
          Future(Left(UserIsNotEnabledToReleaseEventsProblem))
        else {
          val current = journalState.userIdToReleasedEventId.getOrElse(userId, EventId.BeforeFirst)
          if (untilEventId < current)
            Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = untilEventId, currentUntilEventId = current)))
          else
            persist(JournalEventsReleased(userId, untilEventId)) {
              case (Stamped(_,_, _ <-: event), journaledState) =>
                journalState = journalState.applyEvent(event)
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

      case MasterCommand.IssueTestEvent =>
        persist(MasterTestEvent, async = true)((_, _) =>
          Right(MasterCommand.Response.Accepted))

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
        persistTransaction(events.map(KeyedEvent(_))) { (_, _) =>
          setRepo(changedRepo)
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

  private def setRepo(o: Repo): Unit = {
    repo = o
    orderProcessor = new OrderProcessor(repo.idTo[Workflow], idToOrder)
  }

  private def registerAgent(agent: AgentRef, agentRunId: Option[AgentRunId], eventId: EventId): AgentEntry = {
    val actor = watch(actorOf(
      AgentDriver.props(agent.path, agent.uri, agentRunId, eventId = eventId, agentDriverConfiguration, masterConfiguration,
        journalActor = journalActor),
      encodeAsActorName("Agent-" + agent.path.withoutStartingSlash)))
    val entry = AgentEntry(agent, actor)
    agentRunId foreach { o =>
      entry.agentRunId := o
    }
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

  private def addOrderWithUncheckedId(freshOrder: FreshOrder): Future[Checked[Boolean]] = {
    val order = freshOrder.toOrder(repo.versionId)
    orderRegister.get(order.id) match {
      case Some(_) =>
        logger.debug(s"Discarding duplicate added Order: $freshOrder")
        Future.successful(Right(false))

      case None =>
        repo.idTo[Workflow](order.workflowId) match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(workflow) =>
            persist/*Async?*/(order.id <-: OrderAdded(workflow.id, order.state.scheduledFor, order.arguments)) { (stamped, journaledState) =>
              handleOrderEvent(stamped)
              Right(true)
            }
            .flatMap(o => testAddOrderDelay.runToFuture.map(_ => o))  // test only
        }
    }
  }

  private def addOrders(freshOrders: Seq[FreshOrder]): Future[Checked[Completed]] =
    freshOrders.toVector.traverse[Checked, FreshOrder](order => order.id.checkedNameSyntax.map(_ => order))
      .flatMap(_
        .filterNot(o => orderRegister.contains(o.id))  // Ignore known orders
        .map(_.toOrder(repo.versionId))
        .traverse[Checked, (Order[Order.Fresh], Workflow)](order => repo.idTo[Workflow](order.workflowId).map(order -> _))
        .map { ordersAndWorkflows =>
          val events = for ((order, workflow) <- ordersAndWorkflows) yield
            order.id <-: OrderAdded(workflow.id/*reuse*/, order.state.scheduledFor, order.arguments)
          persistMultiple(events) { (stampedEvents, journaledState) =>
            for (o <- stampedEvents) handleOrderEvent(o)
            Completed
          }
        })
      .evert

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

    // When recovering, proceedWithOrderOnMaster may issue the same event multiple times,
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
        persist(order.id <-: OrderAttachable(agentEntry.agentRefPath)) { (stamped, journaledState) =>
          handleOrderEvent(stamped)
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
    final case object GetRepo extends Command
    final case class AddOrder(order: FreshOrder) extends Command
    final case class AddOrders(order: Seq[FreshOrder]) extends Command
    final case class GetOrder(orderId: OrderId) extends Command
    final case object GetOrders extends Command
    final case object GetOrderCount extends Command
    final case object GetState extends Command
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
    var lastAgentEventId: EventId = EventId.BeforeFirst,
    agentRunId: SetOnce[AgentRunId] = SetOnce[AgentRunId],
    var actorTerminated: Boolean = false)
  {
    def agentRefPath = agentRef.path

    def toSnapshot =
      AgentSnapshot(agentRefPath, agentRunId.toOption, lastAgentEventId)

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
