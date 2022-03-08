package js7.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Terminated}
import akka.pattern.ask
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.Problems.{AgentDuplicateOrder, AgentIsShuttingDown}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DetachItem, DetachOrder, MarkOrder, OrderCommand, ReleaseEvents, Response}
import js7.agent.data.event.AgentEvent.{AgentReady, AgentShutDown}
import js7.agent.main.AgentMain
import js7.agent.scheduler.order.AgentOrderKeeper._
import js7.agent.subagent.SubagentKeeper
import js7.base.crypt.{SignatureVerifier, Signed}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.{RichCheckedTask, RichMonixTask}
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.JavaTime._
import js7.base.time.ScalaTime._
import js7.base.time.{AdmissionTimeScheme, AlarmClock, TimeInterval}
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{DuplicateKeyException, SetOnce}
import js7.common.akkautils.Akkas.{encodeAsActorName, uniqueActorName}
import js7.common.akkautils.SupervisorStrategies
import js7.common.utils.Exceptions.wrapException
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.calendar.Calendar
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.{<-:, Event, EventId, JournalState, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.{ExecuteAdmissionTimeSwitch, InstructionExecutorService}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, SignedItemAttachedToMe}
import js7.data.item.{InventoryItem, InventoryItemPath, SignableItem, UnsignedSimpleItem}
import js7.data.job.{JobKey, JobResource}
import js7.data.order.OrderEvent.{OrderAttachedToAgent, OrderBroken, OrderCoreEvent, OrderDetached, OrderProcessed}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.state.{OrderEventHandler, StateView}
import js7.data.subagent.{SubagentId, SubagentRef}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import js7.journal.recover.Recovered
import js7.journal.state.FileStatePersistence
import js7.journal.{JournalActor, MainJournalingActor}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import shapeless.tag

/**
  * Keeper of one Controller's orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  totalRunningSince: Deadline,
  recovered_ : Recovered[AgentState],
  signatureVerifier: SignatureVerifier,
  jobLauncherConf: JobLauncherConf,
  persistence: FileStatePersistence[AgentState],
  private implicit val clock: AlarmClock,
  conf: AgentConfiguration)
  (implicit protected val scheduler: Scheduler, iox: IOExecutor)
extends MainJournalingActor[AgentState, Event]
with Stash
{
  import conf.akkaAskTimeout
  import context.{actorOf, watch}

  private val ownAgentPath = persistence.currentState.meta.agentPath
  private val localSubagentId = persistence.currentState.meta.subagentId
  private val controllerId = persistence.currentState.meta.controllerId
  private implicit val instructionExecutorService = new InstructionExecutorService(clock)

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor = tag[JournalActor.type](persistence.journalActor: ActorRef)
  protected def journalConf = conf.journalConf

  private var journalState = JournalState.empty
  private val jobRegister = mutable.Map.empty[JobKey, JobEntry]
  private val workflowRegister = new WorkflowRegister(ownAgentPath)
  private val fileWatchManager = new FileWatchManager(ownAgentPath, persistence, conf.config)
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

    def start(terminate: AgentCommand.ShutDown): Unit =
      if (!shuttingDown) {
        shutDownCommand = Some(terminate)
        since := now
        fileWatchManager.stop.runAsyncAndForget
        if (terminate.suppressSnapshot) {
          snapshotFinished = true
        } else {
          journalActor ! JournalActor.Input.TakeSnapshot  // Take snapshot before OrderActors are stopped
          stillTerminatingSchedule = Some(scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
            self ! Internal.StillTerminating
          })
        }
        continue()
      }

    def close() = {
      stillTerminatingSchedule foreach (_.cancel())
    }

    def onStillTerminating() =
      logger.info(s"Still terminating, waiting for ${orderRegister.size} orders" +
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
                persistence.stop.runAsyncAndForget
              }
            }
          }
        }
      }
  }
  import shutdown.shuttingDown

  private val subagentKeeper =
    new SubagentKeeper(ownAgentPath, persistence, jobLauncherConf, conf, context.system)

  watch(journalActor)
  self ! Internal.Recover(recovered_)
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
    case Internal.Recover(recovered) =>
      val state = recovered.state
      journalState = state.journalState

      self.forward(Internal.JournalIsReady(state))
      become("Recovering")(recovering(state))
      unstashAll()

    case _ => stash()
  }

  private def recovering(recoveredState: AgentState): Receive = {
    var remainingOrders = recoveredState.idToOrder.size

    def continue() =
      if (remainingOrders == 0) {
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
            totalRunningTime = totalRunningSince.elapsed.roundUpToNext(1.ms))
        ) { (_, _) =>
          self ! Internal.OrdersRecovered(recoveredState)
        }
      }

    val receive: Receive = {
      case Internal.JournalIsReady(state) =>
        logger.info(s"${orderRegister.size} Orders and ${workflowRegister.size} Workflows recovered")
        subagentKeeper
          .initialize(localSubagentId, controllerId)
          .*>(
            subagentKeeper
              .recoverSubagents(recoveredState.idToSubagentRefState.values.toVector)
              .map {
                case Left(problem) => logger.error(
                  s"subagentKeeper.recoverSubagents => $problem")  // ???
                case _ =>
              })
          .materialize
          .foreach { tried =>
            self ! Internal.SubagentKeeperInitialized(state, tried)
          }

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

      case OrderActor.Output.RecoveryFinished(order) =>
        remainingOrders -= 1
        continue()

      case Internal.OrdersRecovered(state) =>
        state.idToOrder.values.view.flatMap(_.ifState[Order.Processing]).foreach { order =>
          for (jobKey <- state.jobKey(order.workflowPosition).toOption) {
            jobRegister(jobKey).recoverProcessingOrder(order)
          }
        }
        subagentKeeper.start
          .runToFuture
          .onComplete { tried =>
            for (t <- tried.failed) logger.error(s"subagentKeeper.start: ${t.toStringWithCauses}")
            self ! Internal.SubagentKeeperStarted
          }

      case Internal.SubagentKeeperStarted =>
        for (order <- persistence.currentState.idToOrder.values) {
          proceedWithOrder(order)
        }
        self ! Internal.Ready

      case Internal.Ready =>
        logger.info(s"Agent '${ownAgentPath.string}' is ready" +
          AgentMain.runningSince.fold("")(o => s" (after ${o.elapsed.pretty})") +
          "\n" + "─" * 80)

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
    case Input.ExternalCommand(cmd, response) =>
      response.completeWith(processCommand(cmd))

    case terminate: AgentCommand.ShutDown =>
      shutdown.start(terminate)
      sender() ! AgentCommand.Response.Accepted

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case OrderActor.Output.OrderChanged(orderId, previousOrderOrNull, events) =>
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
                jobKey <- persistence.currentState.jobKey(previousOrderOrNull.workflowPosition)
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
          proceedWithOrder(orderId)
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

    case DetachItem(path: OrderWatchPath) =>
      fileWatchManager.remove(path)
        .map(_.rightAs(AgentCommand.Response.Accepted))
        .runToFuture

    case DetachItem(itemKey @ (_: InventoryItemPath.AttachableToAgent | WorkflowId.as(_))) =>
      if (!persistence.currentState.keyToItem.contains(itemKey)) {
        logger.warn(s"DetachItem($itemKey) but item is unknown")
        Future.successful(Right(AgentCommand.Response.Accepted))
      } else
        itemKey match {
          case subagentId: SubagentId =>
            subagentKeeper
              .remove(subagentId)
              .flatMapT(_ =>
                persistence
                  .persistKeyedEvent(ItemDetached(itemKey, ownAgentPath))
                  .rightAs(AgentCommand.Response.Accepted)
              )
              .runToFuture

          case _ =>
            persist(ItemDetached(itemKey, ownAgentPath)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }
        }

    case AgentCommand.TakeSnapshot =>
      (journalActor ? JournalActor.Input.TakeSnapshot)
        .mapTo[JournalActor.Output.SnapshotTaken.type]
        .map(_ => Right(AgentCommand.Response.Accepted))

    case _ => Future.successful(Left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}")))  // Should not happen
  }

  private def attachUnsignedItem(item: UnsignedSimpleItem): Future[Checked[Response.Accepted]] =
    item match {
      case fileWatch: FileWatch =>
        if (!conf.scriptInjectionAllowed)
          Future.successful(Left(SignedInjectionNotAllowed))
        else
          fileWatchManager.update(fileWatch)
            .map(_.rightAs(AgentCommand.Response.Accepted))
            .runToFuture

      case item @ (_: Calendar | _: SubagentRef) =>
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

              case Some(registeredWorkflow) if registeredWorkflow.withoutSource == workflow.withoutSource =>
                Future.successful(Right(AgentCommand.Response.Accepted))

              case Some(_) =>
                Future.successful(Left(Problem.pure(s"Different duplicate ${workflow.id}")))
            }

          case jobResource: JobResource =>
            persist(ItemAttachedToMe(jobResource)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }

          case _ =>
            Future.successful(Left(Problem.pure(s"AgentCommand.AttachSignedItem(${signed.value.key}) for unknown SignableItem")))
        }
    }

  private def proceedWithItem(item: InventoryItem): Task[Checked[Unit]] =
    item match {
      case subagentRef: SubagentRef =>
        persistence.state.flatMap(_
          .idToSubagentRefState.get(subagentRef.id)
          .fold(Task.pure(Checked.unit))(subagentRefState => subagentKeeper
            .proceedWithSubagent(subagentRefState)
            .materializeIntoChecked))

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
                    attachOrder(/*workflowRegister.reuseMemory*/(order), workflow)
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
            persistence.currentState.idToOrder.checked(orderId).flatMap(_.detaching) match {
              case Left(problem) => Future.successful(Left(problem))
              case Right(_) =>
                val promise = Promise[Unit]()
                orderEntry.detachResponses ::= promise
                (orderEntry.actor ? OrderActor.Command.HandleEvents(OrderDetached :: Nil))
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
                (orderEntry.actor ? OrderActor.Command.HandleEvents(events))
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
        jobRegister.insert(jobKey -> new JobEntry(jobKey, job, zone))
      }
    }

  private def attachOrder(order: Order[Order.IsFreshOrReady], workflow: Workflow): Future[Completed] = {
    val actor = newOrderActor(order.id)
    orderRegister.insert(order.id, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttachedToAgent
  }

  private def newOrderActor(orderId: OrderId) =
    watch(actorOf(
      OrderActor.props(orderId, subagentKeeper, journalActor = journalActor, journalConf),
      name = uniqueActorName(encodeAsActorName("Order:" + orderId.string))))

  private def handleOrderEvent(
    previousOrder: Order[Order.State],
    event: OrderEvent)
  : Unit = {
    // updatedOrderId may be outdated, changed by more events in the same batch
    // Nevertheless, updateOrderId is the result of the event.
    val orderId = previousOrder.id
    val agentState = persistence.currentState
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
    persistence.currentState.idToOrder.checked(orderId) match {
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
        val keyedEvents = orderEventSource.nextEvents(order.id)
        keyedEvents foreach {
          case KeyedEvent(orderId, OrderBroken(problem)) =>
            logger.error(s"Order ${orderId.string} is broken: $problem")

          case KeyedEvent(orderId_, event) =>
            val future = orderRegister(orderId_).actor ? OrderActor.Command.HandleEvents(event :: Nil)
            try Await.result(future, 99.s) // TODO Blocking! SLOW because inhibits parallelization
            catch { case NonFatal(t) => logger.error(
              s"$orderId_ <-: ${event.toShortString} => ${t.toStringWithCauses}")
            }
            // TODO Not awaiting the response may lead to duplicate events
            //  for example when OrderSuspensionMarked is emitted after OrderProcessed and before OrderMoved.
            //  Then, two OrderMoved are emitted, because the second event is based on the same Order state.
        }
        if (keyedEvents.isEmpty && order.isProcessable) {
          onOrderIsProcessable(order)
        }
      }
    }
  }

  private def onOrderIsProcessable(order: Order[Order.State]): Unit =
    if (!shuttingDown) {
      for (_ <- order.attached.onProblem(p => logger.error(s"onOrderIsProcessable: $p"))) {
        persistence.currentState
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
      }
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
    //new OrderEventSource(persistence.currentState)
    new OrderEventSource(new StateView {
      def isAgent = true

      def controllerId = AgentOrderKeeper.this.controllerId

      def idToOrder = persistence.currentState.idToOrder

      def orders = persistence.currentState.idToOrder.values

      def idToWorkflow = persistence.currentState.idToWorkflow

      def workflowPathToId(workflowPath: WorkflowPath) =
        persistence.currentState.workflowPathToId(workflowPath)

      def pathToLockState = persistence.currentState.pathToLockState

      def pathToBoardState = persistence.currentState.pathToBoardState

      def keyToItem = persistence.currentState.keyToItem
    })

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    final case class ExternalCommand(command: AgentCommand, response: Promise[Checked[Response]])
  }

  private object Internal {
    final case class Recover(recovered: Recovered[AgentState])
    final case class JournalIsReady(agentState: AgentState)
    final case class SubagentKeeperInitialized(agentState: AgentState, tried: Try[Unit])
    final case class OrdersRecovered(agentState: AgentState)
    case object SubagentKeeperStarted
    case object Ready
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

    def -=(orderId: OrderId): Unit =
      remove(orderId)

    def remove(orderId: OrderId, dontWarn: Boolean = false): Unit =
      if (!inProcess.remove(orderId)) {
        val s = queue.size
        queue -= orderId
        if (!dontWarn && queue.size == s) {
          logger.warn(s"JobRegister.OrderQueue: unknown $orderId")
        }
        queueSet -= orderId
      }
  }
}
