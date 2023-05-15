package js7.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Terminated}
import akka.pattern.ask
import com.softwaremill.tagging.{@@, Tagger}
import io.circe.syntax.EncoderOps
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DetachItem, DetachOrder, MarkOrder, OrderCommand, ReleaseEvents, Response}
import js7.agent.data.event.AgentEvent.{AgentReady, AgentShutDown}
import js7.agent.scheduler.order.AgentOrderKeeper.*
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.{SignatureVerifier, Signed}
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.{RichCheckedTask, RichMonixTask}
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.JavaTime.*
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval}
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{Allocated, DuplicateKeyException, SetOnce}
import js7.common.akkautils.Akkas.{encodeAsActorName, uniqueActorName}
import js7.common.akkautils.SupervisorStrategies
import js7.common.system.PlatformInfos.currentPlatformInfo
import js7.common.system.startup.ServiceMain
import js7.common.utils.Exceptions.wrapException
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.agent.AgentRef
import js7.data.agent.Problems.{AgentDuplicateOrder, AgentIsShuttingDown}
import js7.data.calendar.Calendar
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{<-:, Event, EventId, JournalState, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.{ExecuteAdmissionTimeSwitch, InstructionExecutorService}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.{InventoryItem, SignableItem, UnsignedItem}
import js7.data.job.{JobKey, JobResource}
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderCoreEvent, OrderDetached, OrderProcessed}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.state.OrderEventHandler
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowPathControl}
import js7.journal.state.FileJournal
import js7.journal.{JournalActor, MainJournalingActor}
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.subagent.director.SubagentKeeper
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * Keeper of one Controller's orders.
 *
 * @author Joacim Zschimmer
 */
final class AgentOrderKeeper(
  totalRunningSince: Deadline,
  failedOverSubagentId: Option[SubagentId],
  recoveredAgentState : AgentState,
  signatureVerifier: SignatureVerifier,
  journalAllocated: Allocated[Task, FileJournal[AgentState]],
  private implicit val clock: AlarmClock,
  conf: AgentConfiguration,
  testEventBus: StandardEventBus[Any])
  (implicit protected val scheduler: Scheduler, iox: IOExecutor)
extends MainJournalingActor[AgentState, Event]
with Stash
{
  import conf.implicitAkkaAskTimeout
  import context.{actorOf, watch}

  private val journal = journalAllocated.allocatedThing
  private val (ownAgentPath, localSubagentId, controllerId) = {
    val meta = journal.unsafeCurrentState().meta
    val subagentId =
      if (!conf.clusterConf.isBackup)
        meta.directors.headOption
      else if (meta.directors.size == 1) throw new IllegalStateException(
        "Missing definition of backup Subagent in AgentMetaState")
      else
        meta.directors.get(1)
    (meta.agentPath, subagentId, meta.controllerId)
  }
  private implicit val instructionExecutorService: InstructionExecutorService =
    new InstructionExecutorService(clock)

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor: ActorRef @@ JournalActor.type =
    journal.journalActor.taggedWith[JournalActor.type]
  protected def journalConf = conf.journalConf

  private var journalState = JournalState.empty
  private val jobRegister = mutable.Map.empty[JobKey, JobEntry]
  private val workflowRegister = new WorkflowRegister(ownAgentPath)
  private val fileWatchManager = new FileWatchManager(ownAgentPath, journal, conf.config)
  private val orderRegister = new OrderRegister

  private object shutdown {
    private var shutDownCommand: Option[AgentCommand.ShutDown] = None
    private var snapshotFinished = false
    private var stillTerminatingSchedule: Option[Cancelable] = None
    private var terminatingOrders = false
    private var terminatingJobs = false
    private var terminatingJournal = false
    val since = SetOnce[Deadline]

    def shuttingDown = shutDownCommand.isDefined

    def start(cmd: AgentCommand.ShutDown): Unit =
      if (!shuttingDown) {
        if (cmd.isFailover) {
          // Dirty exit
          subagentKeeper.testFailover()
          journalAllocated.stop.runAsyncAndForget
          context.stop(self)
        } else {
          shutDownCommand = Some(cmd)
          since := now
          fileWatchManager.stop.runAsyncAndForget
          if (cmd.suppressSnapshot || cmd.isFailover) {
            snapshotFinished = true
          } else {
            journalActor ! JournalActor.Input.TakeSnapshot  // Take snapshot before OrderActors are stopped
            stillTerminatingSchedule = Some(scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
              self ! Internal.StillTerminating
            })
          }
          continue()
        }
      }

    def close() = {
      stillTerminatingSchedule foreach (_.cancel())
    }

    def onStillTerminating() =
      logger.info(s"🟠 Still terminating, waiting for ${orderRegister.size} orders" +
        s", ${jobRegister.size} jobs" +
        (!snapshotFinished ?? ", and the snapshot"))

    def onSnapshotTaken(): Unit =
      if (shuttingDown) {
        snapshotFinished = true
        continue()
      }

    def continue() =
      for (shutDown <- shutDownCommand) {
        logger.trace(s"termination.continue: ${orderRegister.size} orders, " +
          jobRegister.size + " jobs" +
          (snapshotFinished ?? ", snapshot taken"))
        if (snapshotFinished) {
          if (!terminatingOrders) {
            terminatingOrders = true
            for (o <- orderRegister.values if !o.isDetaching) {
              o.actor ! OrderActor.Input.Terminate(shutDown.processSignal/*only local Subagent*/)
            }
          }
          if (orderRegister.isEmpty) {
            if (!terminatingJobs) {
              terminatingJobs = true
              subagentKeeper.stop
                .onErrorHandle(t => logger.error(
                  s"subagentKeeper.stop =>${t.toStringWithCauses}", t))
                .map(_ => self ! Internal.JobDriverStopped)
                .runAsyncAndForget
            }
            if (jobRegister.isEmpty && !terminatingJournal) {
              persist(AgentShutDown) { (_, _) =>
                terminatingJournal = true
                journalAllocated.stop.runAsyncAndForget
              }
            }
          }
        }
      }
  }
  import shutdown.shuttingDown

  private val subagentKeeper =
    new SubagentKeeper(
      localSubagentId, ownAgentPath, controllerId,
      failedOverSubagentId,
      journal, conf.subagentDirectorConf,
      iox, context.system, testEventBus)

  watch(journalActor)
  self ! Internal.Recover(recoveredAgentState)
  // Do not use recovered_ after here to allow release of the big object

  override def postStop() = {
    // TODO Use Resource (like the Subagent starter)
    // TODO Blocking!
    Try(subagentKeeper.stop.uncancelable.timeout(3.s).logWhenItTakesLonger.await(99.s))

    fileWatchManager.stop.runAsyncAndForget
    shutdown.close()
    super.postStop()
    logger.debug("Stopped" + shutdown.since.toOption.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))
  }

  def receive = {
    case Internal.Recover(recoveredAgentState) =>
      journalState = recoveredAgentState.journalState

      become("Recovering")(recovering(recoveredAgentState))
      unstashAll()

      subagentKeeper.start
        .*>(journal.state.flatMap(state => subagentKeeper.recoverSubagents(
          state.idToSubagentItemState.values.toVector)))
        .flatMapT(_ =>
          subagentKeeper.recoverSubagentSelections(
            recoveredAgentState.pathToUnsignedSimple(SubagentSelection).values.toVector))
        .map(_.orThrow)
        .materialize
        .foreach { tried =>
          self.forward(Internal.SubagentKeeperInitialized(recoveredAgentState, tried))
        }

    case _ => stash()
  }

  private def recovering(recoveredState: AgentState): Receive = {
    var remainingOrders = recoveredState.idToOrder.size

    def continue() =
      if (remainingOrders == 0) {
        subagentKeeper.continueDetaching
          .logWhenItTakesLonger("subagentKeeper.continueDetaching")
          .awaitInfinite

        if (!journalState.userIdToReleasedEventId.contains(controllerId.toUserId)) {
          // Automatically add Controller's UserId to list of users allowed to release events,
          // to avoid deletion of journal files due to an empty list, before controller has read the events.
          // The controller has to send ReleaseEvents commands to release obsolete journal files.
          persist(JournalEventsReleased(controllerId.toUserId, EventId.BeforeFirst)) {
            case (Stamped(_,_, _ <-: event), journaledState) =>
              journalState = journalState.applyEvent(event)
          }
        }

        fileWatchManager.start
          .map(_.orThrow)  // How to handle a failure, due to missing environment variable ???
          .runAsyncAndForget

        // TODO AgentReady should be the first event ?
        persist(
          AgentReady(
            ZoneId.systemDefault.getId,
            totalRunningTime = totalRunningSince.elapsed.roundUpToNext(1.ms),
            Some(currentPlatformInfo()))
        ) { (_, _) =>
          self ! Internal.OrdersRecovered(recoveredState)
        }
      }

    val receive: Receive = {
      case Internal.SubagentKeeperInitialized(state, tried) =>
        for (t <- tried.failed) throw t.appendCurrentStackTrace

        for (workflow <- state.idToWorkflow.values)
          wrapException(s"Error while recovering ${workflow.path}") {
            workflowRegister.recover(workflow)
            val timeZone = ZoneId.of(workflow.timeZone.string) // throws on unknown time zone !!!
            createJobEntries(workflow, timeZone)
          }

        for (order <- state.idToOrder.values)
          wrapException(s"Error while recovering ${order.id}") {
            //val order = workflowRegister.reuseMemory(recoveredOrder)
            val actor = newOrderActor(order.id)
            orderRegister.recover(order.id, actor)
            actor ! OrderActor.Input.Recover(order)
          }

        continue()

      case OrderActor.Output.RecoveryFinished =>
        remainingOrders -= 1
        continue()

      case Internal.OrdersRecovered(state) =>
        for (order <- state.idToOrder.values.view.flatMap(_.ifState[Order.Processing])) {
          for (jobKey <- state.jobKey(order.workflowPosition).toOption) {
            jobRegister(jobKey).recoverProcessingOrder(order)
          }
        }

        // proceedWithOrder before subagentKeeper.start because continued Orders (still
        // processing at remote Subagent) will emit events and change idToOrder asynchronously!
        // But not before SubagentKeeper has been started (when Subagents are coupled).
        for (order <- state.idToOrder.values) {
          proceedWithOrder(order)
        }

        logger.info(ServiceMain.readyMessageWithLine(s"$ownAgentPath is ready"))
        become("ready")(ready)
        unstashAll()

      case _: AgentCommand.ShutDown =>
        logger.info("Command 'ShutDown' terminates Agent while recovering")
        context.stop(self)
        sender() ! AgentCommand.Response.Accepted

      case _ =>
        stash()
    }
    receive
  }

  private def ready: Receive = {
    case Input.ExternalCommand(cmd, correlId, response) =>
      response.completeWith(
        correlId.bind {
          processCommand(cmd)
        })

    case cmd: AgentCommand.ShutDown =>
      shutdown.start(cmd)
      sender() ! AgentCommand.Response.Accepted

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case OrderActor.Output.OrderChanged(orderId, correlId, previousOrderOrNull, events) =>
      correlId.bind[Unit] {
        if (!shuttingDown) {
          // previousOrderOrNull is null only for OrderAttachedToAgent event
          var order = previousOrderOrNull
          var myJobEntry: JobEntry = null
          for (event <- events) {
            event match {
              case event: OrderAttachedToAgent =>
                order = Order.fromOrderAttached(orderId, event)

              case event: OrderProcessed =>
                (for {
                  jobKey <- journal.unsafeCurrentState().jobKey(previousOrderOrNull.workflowPosition)
                  jobEntry <- jobRegister.checked(jobKey)
                } yield jobEntry)
                match {
                  case Left(problem) =>
                    logger.error(s"OrderActor.Output.OrderChanged($orderId) => $problem")

                  case Right(jobEntry) =>
                    jobEntry.taskCount -= 1
                    myJobEntry = jobEntry
                }
                order = order.applyEvent(event).orThrow

              case event: OrderCoreEvent =>
                order = order.applyEvent(event).orThrow

              case _ =>
            }
            handleOrderEvent(order, event)
          }
          if (myJobEntry != null) tryStartProcessing(myJobEntry)
          if (!events.lastOption.contains(OrderDetached)) {
            proceedWithOrder(order)
          }
        }
      }

    case Internal.Due(orderId) if orderRegister contains orderId =>
      proceedWithOrder(orderId)

    case Internal.JobDue(jobKey) =>
      for (jobEntry <- jobRegister.get(jobKey)) {
        tryStartProcessing(jobEntry)
      }
  }

  private def processCommand(cmd: AgentCommand): Future[Checked[Response]] = cmd match {
    case cmd: OrderCommand => processOrderCommand(cmd)

    case AttachItem(item) =>
      attachUnsignedItem(item)

    case AttachSignedItem(signed: Signed[SignableItem]) =>
      attachSignedItem(signed)

    case DetachItem(itemKey) if itemKey.isAssignableToAgent =>
      if (!journal.unsafeCurrentState().keyToItem.contains(itemKey)) {
        logger.warn(s"DetachItem($itemKey) but item is unknown")
        Future.successful(Right(AgentCommand.Response.Accepted))
      } else
        itemKey match {
          case path: OrderWatchPath =>
            fileWatchManager.remove(path)
              .rightAs(AgentCommand.Response.Accepted)
              .runToFuture

          case subagentId: SubagentId =>
            journal
              .persistKeyedEvent(NoKey <-: ItemDetachingFromMe(subagentId))
              .flatMapT(_ => subagentKeeper
                .startRemoveSubagent(subagentId)
                // SubagentKeeper will emit ItemDetached event
                .map(Right(_)))
              .as(Right(AgentCommand.Response.Accepted))
              .runToFuture

          case selectionId: SubagentSelectionId =>
            journal
              .persistKeyedEvent(NoKey <-: ItemDetached(selectionId, ownAgentPath))
              .flatMapT(_ => subagentKeeper
                .removeSubagentSelection(selectionId)
                .as(Right(AgentCommand.Response.Accepted)))
              .runToFuture

          case _ =>
            persist(ItemDetached(itemKey, ownAgentPath)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }
        }

    case AgentCommand.ResetSubagent(subagentId, force) =>
      subagentKeeper.startResetSubagent(subagentId, force)
        .rightAs(AgentCommand.Response.Accepted)
        .runToFuture

    case AgentCommand.TakeSnapshot =>
      (journalActor ? JournalActor.Input.TakeSnapshot)
        .mapTo[JournalActor.Output.SnapshotTaken.type]
        .map(_ => Right(AgentCommand.Response.Accepted))

    case _ => Future.successful(Left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}")))  // Should not happen
  }

  private def attachUnsignedItem(item: UnsignedItem): Future[Checked[Response.Accepted]] =
    item match {
      case agentRef: AgentRef =>
        if (agentRef.path != ownAgentPath)
          Future.successful(Left(Problem(s"Alien AgentRef(${agentRef.path})")))
        else
          persist(ItemAttachedToMe(agentRef)) { (stampedEvent, journaledState) =>
            proceedWithItem(agentRef).runToFuture
          }.flatten
            .rightAs(AgentCommand.Response.Accepted)

      case fileWatch: FileWatch =>
        if (!conf.scriptInjectionAllowed)
          Future.successful(Left(SignedInjectionNotAllowed))
        else
          fileWatchManager.update(fileWatch)
            .map(_.rightAs(AgentCommand.Response.Accepted))
            .runToFuture

      case item @ (_: Calendar | _: SubagentItem | _: SubagentSelection |
                   _: WorkflowPathControl | _: WorkflowControl) =>
        persist(ItemAttachedToMe(item)) { (stampedEvent, journaledState) =>
          proceedWithItem(item).runToFuture
        }.flatten
          .rightAs(AgentCommand.Response.Accepted)

      case _ =>
        Future.successful(Left(Problem.pure(s"AgentCommand.AttachItem(${item.key}) for unknown InventoryItem")))
    }

  private def attachSignedItem(signed: Signed[SignableItem]): Future[Checked[Response.Accepted]] =
    signatureVerifier.verify(signed.signedString) match {
      case Left(problem) => Future.successful(Left(problem))
      case Right(signerIds) =>
        logger.info(Logger.SignatureVerified, s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")

        signed.value match {
          case workflow: Workflow =>
            workflowRegister.get(workflow.id) match {
              case None =>
                workflow.timeZone.toZoneId match {
                  case Left(problem) => Future.successful(Left(problem))
                  case Right(zoneId) =>
                    persist(SignedItemAttachedToMe(signed)) { (stampedEvent, journaledState) =>
                      val reducedWorkflow = journaledState.idToWorkflow(workflow.id)
                      workflowRegister.handleEvent(stampedEvent.value, reducedWorkflow)
                      createJobEntries(reducedWorkflow, zoneId)
                      Right(AgentCommand.Response.Accepted)
                    }
                }

              case Some(registeredWorkflow) =>
                Future.successful(
                  if (workflow.withoutSource.reduceForAgent(ownAgentPath) != registeredWorkflow.withoutSource) {
                    logger.warn(s"AttachSignedItem: Different duplicate ${workflow.id}:")
                    logger.warn(s"AttachSignedItem  ${workflow.withoutSource.asJson.toPrettyString}")
                    logger.warn(s"But registered is ${registeredWorkflow.withoutSource.asJson.toPrettyString}")
                    Left(Problem.pure(s"Different duplicate ${workflow.id}"))
                  } else
                    Right(AgentCommand.Response.Accepted))
            }

          case _: JobResource =>
            persist(SignedItemAttachedToMe(signed)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }

          case _ =>
            Future.successful(Left(Problem.pure(s"AgentCommand.AttachSignedItem(${signed.value.key}) for unknown SignableItem")))
        }
    }

  private def proceedWithItem(item: InventoryItem): Task[Checked[Unit]] =
    item match {
      case subagentItem: SubagentItem =>
        journal.state.flatMap(_
          .idToSubagentItemState.get(subagentItem.id)
          .fold(Task.pure(Checked.unit))(subagentItemState => subagentKeeper
            .proceedWithSubagent(subagentItemState)
            .materializeIntoChecked))

      case subagentSelection: SubagentSelection =>
        subagentKeeper.addOrReplaceSubagentSelection(subagentSelection)

      case workflowPathControl: WorkflowPathControl =>
        if (!workflowPathControl.suspended) {
          // Slow !!!
          for (order <- journal.unsafeCurrentState().orders
               if order.workflowPath == workflowPathControl.workflowPath) {
            proceedWithOrder(order)
          }
        }
        Task.right(())

      case _ => Task.right(())
    }

  private def processOrderCommand(cmd: OrderCommand): Future[Checked[Response]] = cmd match {
    case AttachOrder(order) =>
      if (shuttingDown)
        Future.successful(Left(AgentIsShuttingDown))
      else
        order.attached match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(agentPath) =>
            if (agentPath != ownAgentPath)
              Future.successful(Left(Problem(s"Wrong $agentPath")))
            else
              workflowRegister.get(order.workflowId) match {
                case None =>
                  Future.successful(Left(Problem.pure(s"Unknown ${order.workflowId}")))
                case Some(workflow) =>
                  if (!workflow.isDefinedAt(order.position))
                    Future.successful(Left(Problem.pure(s"Unknown Position ${order.workflowPosition}")))
                  else if (orderRegister contains order.id)
                    Future.successful(Left(AgentDuplicateOrder(order.id)))
                  else if (shuttingDown)
                    Future.successful(Left(AgentIsShuttingDown))
                  else
                    attachOrder(/*workflowRegister.reuseMemory*/(order))
                      .map((_: Completed) => Right(Response.Accepted))
              }
        }

    case DetachOrder(orderId) =>
      if (shuttingDown)
        Future.successful(AgentIsShuttingDown)
      else
        orderRegister.get(orderId) match {
          case Some(orderEntry) =>
            // TODO Antwort erst nach OrderDetached _und_ Terminated senden, wenn Actor aus orderRegister entfernt worden ist
            // Bei langsamem Agenten, schnellem Controller-Wiederanlauf kann DetachOrder doppelt kommen, während OrderActor sich noch beendet.
            journal.unsafeCurrentState().idToOrder.checked(orderId).flatMap(_.detaching) match {
              case Left(problem) => Future.successful(Left(problem))
              case Right(_) =>
                val promise = Promise[Unit]()
                orderEntry.detachResponses ::= promise
                (orderEntry.actor ? OrderActor.Command.HandleEvents(OrderDetached :: Nil, CorrelId.current))
                  .mapTo[Completed]
                  .onComplete {
                    case Failure(t) => promise.tryFailure(t)
                    case Success(Completed) =>
                      // Ignore this and instead await OrderActor termination and removal from orderRegister.
                      // Otherwise in case of a quick Controller restart, CoupleController would response with this OrderId
                      // and the Controller will try again to DetachOrder, while the original DetachOrder is still in progress.
                  }
                promise.future.map(_ => Right(AgentCommand.Response.Accepted))
            }
          case None =>
            // May occur after Controller restart when Controller is not sure about order has been detached previously.
            logger.debug(s"Ignoring duplicate $cmd")
            Future.successful(Right(AgentCommand.Response.Accepted))
        }

    case MarkOrder(orderId, mark) =>
      orderRegister.checked(orderId) match {
        case Left(problem) =>
          Future.failed(problem.throwable)
        case Right(orderEntry) =>
          if (orderEntry.isDetaching)
            Future.successful(Right(AgentCommand.Response.Accepted))
          else
            orderEventSource.markOrder(orderId, mark) match {
              case Left(problem) => Future.failed(problem.throwable)
              case Right(None) => Future.successful(Right(AgentCommand.Response.Accepted))
              case Right(Some(events)) =>
                // Several MarkOrder in sequence are not properly handled
                // one after the other because execution is asynchronous.
                // A second command may may see the same not yet updated order.
                // TODO Queue for each order? And no more OrderActor?
                (orderEntry.actor ? OrderActor.Command.HandleEvents(events, CorrelId.current))
                  .mapTo[Completed]
                  .map(_ => Right(AgentCommand.Response.Accepted))
            }
      }

    case ReleaseEvents(after) =>
      if (shuttingDown)
        Future.failed(AgentIsShuttingDown.throwable)
      else {
        val userId = controllerId.toUserId
        val current = journalState.userIdToReleasedEventId(userId)  // Must contain userId
        if (after < current)
          Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = after, currentUntilEventId = current)))
        else
          persist(JournalEventsReleased(userId, after)) {
            case (Stamped(_,_, _ <-: event), journaledState) =>
              journalState = journalState.applyEvent(event)
              Right(AgentCommand.Response.Accepted)
          }
      }
  }

  private def createJobEntries(workflow: Workflow, zone: ZoneId): Unit =
    for ((jobKey, job) <- workflow.keyToJob) {
      if (job.agentPath == ownAgentPath) {
        jobRegister.insert(jobKey, new JobEntry(jobKey, job, zone))
      }
    }

  private def attachOrder(order: Order[Order.IsFreshOrReady]): Future[Completed] = {
    val actor = newOrderActor(order.id)
    orderRegister.insert(order.id, actor)
    (actor ? OrderActor.Command.Attach(order, CorrelId.current)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttachedToAgent
  }

  private def newOrderActor(orderId: OrderId) =
    watch(actorOf(
      OrderActor.props(
        orderId, CorrelId.current, subagentKeeper, journalActor = journalActor, journalConf),
      name = uniqueActorName(encodeAsActorName("Order:" + orderId.string))))

  private def handleOrderEvent(
    previousOrder: Order[Order.State],
    event: OrderEvent)
  : Unit = {
    // updatedOrderId may be outdated, changed by more events in the same batch
    // Nevertheless, updateOrderId is the result of the event.
    val orderId = previousOrder.id
    val agentState = journal.unsafeCurrentState()
    val orderEventHandler = new OrderEventHandler(agentState.idToWorkflow.checked)

    orderEventHandler.handleEvent(previousOrder, event)
      .onProblem(problem => logger.error(
        s"handleOrderEvent($orderId <-: ${event.getClass.simpleScalaName}) => $problem)"))
      .foreach(_ foreach {
        case FollowUp.LeaveJob(jobKey) =>
          jobRegister
            .checked(jobKey)
            .onProblem(problem => logger.error(
              s"handleOrderEvent($orderId <-: ${event.getClass.simpleScalaName}) => $problem)"))
            .foreach { jobEntry =>
              jobEntry.queue.remove(orderId, dontWarn = true)
            }

        case FollowUp.AddChild(childOrder) =>
          val actor = newOrderActor(childOrder.id)
          orderRegister.insert(childOrder.id, actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder)

        case FollowUp.Delete(deleteOrderId) =>
          deleteOrder(deleteOrderId)
      })
  }

  private def proceedWithOrder(orderId: OrderId): Unit =
    journal.unsafeCurrentState().idToOrder.checked(orderId) match {
      case Left(problem) => logger.error(s"Internal: proceedWithOrder($orderId) => $problem")
      case Right(order) => proceedWithOrder(order)
    }

  private def proceedWithOrder(order: Order[Order.State]): Unit = {
    if (order.isAttached) {
      val delayed = clock.lock {
        order.maybeDelayedUntil match {
          case Some(until) if clock.now() < until =>
            // TODO Schedule only the next order ?
            val orderEntry = orderRegister(order.id)
            orderEntry.timer := clock.scheduleAt(until) {
              self ! Internal.Due(order.id)
            }
            true

          case _ =>
            false
        }
      }
      if (!delayed) {
        val agentState = journal.unsafeCurrentState()
        val oes = new OrderEventSource(agentState)
        if (order != agentState.idToOrder(order.id)) {
          // FIXME order should be equal !
          logger.debug(s"🔥 ERROR order    =$order")
          logger.debug(s"🔥 ERROR idToOrder=${agentState.idToOrder(order.id)}")
          //assertThat(oes.state.idToOrder(order.id) == order)
        }
        val keyedEvents = oes.nextEvents(order.id)
        keyedEvents foreach { case KeyedEvent(orderId_, event) =>
          val future = orderRegister(orderId_).actor ?
            OrderActor.Command.HandleEvents(event :: Nil, CorrelId.current)
          try Await.result(future, 99.s) // TODO Blocking! SLOW because inhibits parallelization
          catch { case NonFatal(t) => logger.error(
            s"$orderId_ <-: ${event.toShortString} => ${t.toStringWithCauses}")
          }
          // TODO Not awaiting the response may lead to duplicate events
          //  for example when OrderSuspensionMarked is emitted after OrderProcessed and before OrderMoved.
          //  Then, two OrderMoved are emitted, because the second event is based on the same Order state.
        }
        if (keyedEvents.isEmpty
          && journal.unsafeCurrentState().isOrderProcessable(order)
          && order.isAttached
          && !shuttingDown) {
          onOrderIsProcessable(order)
        }
      }
    }
  }

  private def onOrderIsProcessable(order: Order[Order.State]): Unit =
    journal.unsafeCurrentState()
      .idToWorkflow.checked(order.workflowId)
      .map(workflow => workflow -> workflow.instruction(order.position))
      .match_ {
        case Left(problem) =>
          logger.error(s"onOrderIsProcessable => $problem")

        case Right((workflow, execute: Execute)) =>
          val checkedJobKey = execute match {
            case _: Execute.Anonymous => Right(workflow.anonymousJobKey(order.workflowPosition))
            case o: Execute.Named     => workflow.jobKey(order.position.branchPath, o.name)  // defaultArguments are extracted later
          }
          checkedJobKey
            .flatMap(jobRegister.checked)
            .onProblem(problem =>
              logger.error(s"Internal: onOrderIsProcessable(${order.id}) => $problem"))
            .foreach { jobEntry =>
              onOrderAvailableForJob(order.id, jobEntry)
            }

        case Right(_) =>
      }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit =
  // TODO Make this more functional!
    if (!jobEntry.queue.isKnown(orderId)) {
      jobEntry.queue += orderId
      tryStartProcessing(jobEntry)
    }

  private def tryStartProcessing(jobEntry: JobEntry): Unit = {
    val isEnterable = jobEntry.checkAdmissionTimeInterval(clock) {
      self ! Internal.JobDue(jobEntry.jobKey)
    }
    if (isEnterable) {
      while (jobEntry.isBelowParallelismLimit && jobEntry.queue.nonEmpty) {
        for (orderId <- jobEntry.queue.dequeue()) {
          startProcessing(orderId, jobEntry)
        }
      }
    }
  }

  private def startProcessing(orderId: OrderId, jobEntry: JobEntry): Unit =
    orderRegister.checked(orderId) match {
      case Left(problem) =>
        logger.error(s"onOrderIsProcessable => $problem")

      case Right(orderEntry) =>
        jobEntry.taskCount += 1
        orderEntry.actor ! OrderActor.Input.StartProcessing
    }

  private def deleteOrder(orderId: OrderId): Unit =
    for (orderEntry <- orderRegister.get(orderId)) {
      orderEntry.actor ! OrderActor.Input.Terminate()
      orderRegister.remove(orderId)
    }

  override def unhandled(message: Any) =
    message match {
      case Internal.JobDriverStopped =>
        logger.trace("Internal.JobDriverStopped")
        jobRegister.values.foreach(_.close())
        jobRegister.keys.toVector.foreach(jobRegister.remove)
        shutdown.continue()

      case Terminated(actorRef) if orderRegister contains actorRef =>
        val orderEntry = orderRegister(actorRef)
        val orderId = orderEntry.orderId
        logger.debug(s"Actor '$orderId' stopped")
        for (p <- orderEntry.detachResponses) p.trySuccess(())
        orderRegister.onActorTerminated(actorRef)  // Delete the OrderEntry
        shutdown.continue()

      case Terminated(`journalActor`) if shuttingDown =>
        context.stop(self)

      case Internal.StillTerminating =>
        shutdown.onStillTerminating()

      case _ =>
        super.unhandled(message)
    }

  private def orderEventSource =
    new OrderEventSource(journal.unsafeCurrentState())

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    final case class ExternalCommand(
      command: AgentCommand,
      correlId: CorrelId,
      response: Promise[Checked[Response]])
  }

  private object Internal {
    final case class Recover(agentState: AgentState)
    final case class SubagentKeeperInitialized(agentState: AgentState, tried: Try[Unit])
    final case class OrdersRecovered(agentState: AgentState)
    final case class Due(orderId: OrderId)
    final case class JobDue(jobKey: JobKey)
    case object JobDriverStopped
    object StillTerminating extends DeadLetterSuppression
  }

  private final class JobEntry(
    val jobKey: JobKey,
    val workflowJob: WorkflowJob,
    zone: ZoneId)
  {
    val queue = new OrderQueue
    private val admissionTimeIntervalSwitch = new ExecuteAdmissionTimeSwitch(
      workflowJob.admissionTimeScheme.getOrElse(AdmissionTimeScheme.always),
      zone,
      onSwitch = to =>
        if (!to.contains(TimeInterval.Always)) {
          logger.debug(s"$jobKey: Next admission: ${to getOrElse "None"} $zone")
        })

    var taskCount = 0

    def close(): Unit =
      admissionTimeIntervalSwitch.cancel()

    def recoverProcessingOrder(order: Order[Order.Processing]): Unit = {
      taskCount += 1
      queue.recoverProcessingOrder(order)
    }

    def checkAdmissionTimeInterval(clock: AlarmClock)(onPermissionGranted: => Unit): Boolean =
      admissionTimeIntervalSwitch.updateAndCheck(onPermissionGranted)(clock)

    def isBelowParallelismLimit =
      taskCount < workflowJob.parallelism
  }

  final class OrderQueue private[order] {
    private val queue = mutable.ListBuffer.empty[OrderId]
    private val queueSet = mutable.Set.empty[OrderId]
    private val inProcess = mutable.Set.empty[OrderId]

    def isEmpty = queue.isEmpty
    def nonEmpty = !isEmpty

    def isKnown(orderId: OrderId) =
      queueSet.contains(orderId) || inProcess.contains(orderId)

    def dequeue(): Option[OrderId] =
      queue.nonEmpty option {
        val orderId = queue.remove(0)
        queueSet -= orderId
        inProcess += orderId
        orderId
      }

    def +=(orderId: OrderId) = {
      if (inProcess(orderId)) throw new DuplicateKeyException(s"Duplicate $orderId")
      if (queueSet contains orderId) throw new DuplicateKeyException(s"Duplicate $orderId")
      queue += orderId
      queueSet += orderId
    }

    def recoverProcessingOrder(order: Order[Order.Processing]): Unit =
      inProcess += order.id

    def remove(orderId: OrderId, dontWarn: Boolean = false): Unit =
      if (!inProcess.remove(orderId)) {
        val s = queue.size
        queue -= orderId
        if (!dontWarn && queue.size == s) {
          logger.warn(s"JobRegister.OrderQueue: unknown $orderId")
        }
        queueSet -= orderId
      }

    override def toString = s"OrderQueue(${queue.size} orders, ${inProcess.size} in process)"
  }
}
