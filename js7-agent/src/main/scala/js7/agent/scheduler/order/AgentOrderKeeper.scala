package js7.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Terminated}
import akka.pattern.ask
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.Problems.{AgentDuplicateOrder, AgentIsShuttingDown}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DetachItem, DetachOrder, GetOrder, GetOrderIds, GetOrders, MarkOrder, OrderCommand, ReleaseEvents, Response}
import js7.agent.data.event.AgentEvent.AgentReady
import js7.agent.scheduler.job.JobActor
import js7.agent.scheduler.order.AgentOrderKeeper._
import js7.agent.scheduler.order.JobRegister.JobEntry
import js7.agent.scheduler.order.OrderRegister.OrderEntry
import js7.base.crypt.{SignatureVerifier, Signed}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.ops._
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime._
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.common.akkautils.Akkas.{encodeAsActorName, uniqueActorName}
import js7.common.akkautils.SupervisorStrategies
import js7.common.http.CirceToYaml.ToYamlString
import js7.common.utils.Exceptions.wrapException
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.{<-:, Event, EventId, JournalState, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.execution.workflow.{OrderEventHandler, OrderEventSource}
import js7.data.item.BasicItemEvent.{ItemAttachedToAgent, ItemDetached}
import js7.data.item.SignableItem
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderBroken, OrderDetached}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.Execute
import js7.data.workflow.{Workflow, WorkflowId}
import js7.executor.configuration.JobExecutorConf
import js7.executor.configuration.Problems.SignedInjectionNotAllowed
import js7.journal.recover.Recovered
import js7.journal.state.JournaledStatePersistence
import js7.journal.{JournalActor, MainJournalingActor}
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}
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
  executorConf: JobExecutorConf,
  persistence: JournaledStatePersistence[AgentState],
  conf: AgentConfiguration)
  (implicit protected val scheduler: Scheduler, iox: IOExecutor)
extends MainJournalingActor[AgentState, Event]
with Stash
{
  import conf.akkaAskTimeout
  import context.{actorOf, watch}

  private val alarmClock = AlarmClock(
    conf.config.getDuration("js7.time.clock-setting-check-interval").toFiniteDuration)
  private val ownAgentPath = persistence.currentState.meta.agentPath
  private val controllerId = persistence.currentState.meta.controllerId

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor = tag[JournalActor.type](persistence.journalActor: ActorRef)
  protected def journalConf = conf.journalConf

  private var journalState = JournalState.empty
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val fileWatchManager = new FileWatchManager(ownAgentPath, persistence, conf.config)
  private val orderActorConf = OrderActor.Conf(conf.config, conf.journalConf)
  private val orderRegister = new OrderRegister
  private val orderEventHandler = new OrderEventHandler(
    workflowRegister.idToWorkflow.checked,
    orderRegister.idToOrder.checked)

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
      logger.info(s"Still terminating, waiting for ${orderRegister.size} orders, ${jobRegister.size} jobs" +
        (!snapshotFinished ?? ", and the snapshot"))

    def onSnapshotTaken(): Unit =
      if (shuttingDown) {
        snapshotFinished = true
        continue()
      }

    def continue() =
      for (terminate <- shutDownCommand) {
        logger.trace(s"termination.continue: ${orderRegister.size} orders, ${jobRegister.size} jobs ${if (snapshotFinished) ", snapshot taken" else ""}")
        if (snapshotFinished) {
          if (!terminatingOrders) {
            terminatingOrders = true
            for (o <- orderRegister.values if !o.isDetaching) {
              o.actor ! OrderActor.Input.Terminate(terminate.processSignal)
            }
          }
          if (orderRegister.isEmpty) {
            if (!terminatingJobs) {
              terminatingJobs = true
              for (a <- jobRegister.values) a.actor ! JobActor.Input.Terminate()
            }
            if (jobRegister.isEmpty && !terminatingJournal) {
              terminatingJournal = true
              persistence.stop.runAsyncAndForget
            }
          }
        }
      }
  }
  import shutdown.shuttingDown

  watch(journalActor)
  self ! Internal.Recover(recovered_)
  // Do not use recovered_ after here to allow release of the big object

  override def postStop() = {
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

        persist(AgentReady(ZoneId.systemDefault.getId, totalRunningTime = totalRunningSince.elapsed)) { (_, _) =>
          become("ready")(ready)
          unstashAll()
          logger.info(s"Agent '${ownAgentPath.string}' is ready")
        }
        fileWatchManager.start()
          .runAsyncAndForget
      }

    val receive: Receive = {
      case OrderActor.Output.RecoveryFinished(order) =>
        orderRegister(order.id).order = order
        proceedWithOrder(order.id)
        remainingOrders -= 1
        continue()

      case Internal.JournalIsReady(state) =>
        logger.info(s"${orderRegister.size} Orders and ${workflowRegister.size} Workflows recovered")

        for (workflow <- state.idToWorkflow.values)
          wrapException(s"Error while recovering ${workflow.path}") {
            workflowRegister.recover(workflow)
            startJobActors(workflow)
          }

        for (recoveredOrder <- state.idToOrder.values)
          wrapException(s"Error while recovering ${recoveredOrder.id}") {
            val order = workflowRegister.reuseMemory(recoveredOrder)
            val workflow = workflowRegister(order.workflowId)  // Workflow is expected to be recovered
            val actor = newOrderActor(order, workflow)
            orderRegister.recover(order, workflow, actor)
            actor ! OrderActor.Input.Recover(order)
          }

        continue()

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

    case OrderActor.Output.OrderChanged(order, events) if orderRegister contains order.id =>
      if (!shuttingDown) {
        for (event <- events) handleOrderEvent(order, event)
        proceedWithOrder(order.id)
      }

    case JobActor.Output.ReadyForOrder if jobRegister contains sender() =>
      if (!shuttingDown) {
        tryStartProcessing(jobRegister(sender()))
      }

    case Internal.Due(orderId) if orderRegister contains orderId =>
      proceedWithOrder(orderId)
  }

  private def processCommand(cmd: AgentCommand): Future[Checked[Response]] = cmd match {
    case cmd: OrderCommand => processOrderCommand(cmd)

    case AttachItem(fileWatch: FileWatch) =>
      if (!conf.scriptInjectionAllowed)
        Future.successful(Left(SignedInjectionNotAllowed))
      else
        fileWatchManager.update(fileWatch)
          .map(_.rightAs(AgentCommand.Response.Accepted))
          .runToFuture

    case AttachSignedItem(signed: Signed[SignableItem]) =>
      attachSignedItem(signed)

    case DetachItem(path: OrderWatchPath) =>
      fileWatchManager.remove(path)
        .map(_.rightAs(AgentCommand.Response.Accepted))
        .runToFuture

    case DetachItem(itemKey @ (_: JobResourcePath | WorkflowId.as(_))) =>
      if (!persistence.currentState.keyToItem.contains(itemKey)) {
        logger.warn(s"DetachItem($itemKey) but item is unknown (okay after Controller restart)")
        Future.successful(Right(AgentCommand.Response.Accepted))
      } else
        persist(ItemDetached(itemKey, ownAgentPath)) { (stampedEvent, journaledState) =>
          Right(AgentCommand.Response.Accepted)
        }

    case AgentCommand.TakeSnapshot =>
      (journalActor ? JournalActor.Input.TakeSnapshot)
        .mapTo[JournalActor.Output.SnapshotTaken.type]
        .map(_ => Right(AgentCommand.Response.Accepted))

    case _ => Future.successful(Left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}")))  // Should not happen
  }

  private def attachSignedItem(signed: Signed[SignableItem]): Future[Checked[Response.Accepted]] =
    signatureVerifier.verify(signed.signedString) match {
      case Left(problem) => Future.successful(Left(problem))
      case Right(signerIds) =>
        logger.info(Logger.SignatureVerified, s"Verified ${signed.value.key}, signed by ${signerIds.mkString(", ")}")

        signed.value match {
          case origWorkflow: Workflow =>
            val workflow = origWorkflow.reduceForAgent(ownAgentPath)
            workflowRegister.get(workflow.id) match {
              case None =>
                logger.trace("Reduced workflow: ⏎\n" + workflow.toYamlString)
                persist(ItemAttachedToAgent(workflow)) { (stampedEvent, journaledState) =>
                  workflowRegister.handleEvent(stampedEvent.value)
                  startJobActors(workflow)
                  Right(AgentCommand.Response.Accepted)
                }

              case Some(registeredWorkflow) if registeredWorkflow.withoutSource == workflow.withoutSource =>
                Future.successful(Right(AgentCommand.Response.Accepted))

              case Some(_) =>
                Future.successful(Left(Problem.pure(s"Different duplicate ${workflow.id}")))
            }

          case jobResource: JobResource =>
            persist(ItemAttachedToAgent(jobResource)) { (stampedEvent, journaledState) =>
              Right(AgentCommand.Response.Accepted)
            }

          case _ =>
            Future.successful(Left(Problem.pure(s"AgentCommand.AttachedItem(${signed.value.key}) for unknown SignableItem")))
        }
    }

  private def processOrderCommand(cmd: OrderCommand): Future[Checked[Response]] = cmd match {
    case AttachOrder(order) =>
      if (shuttingDown)
        Future.failed(AgentIsShuttingDown.throwable)
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
                    attachOrder(workflowRegister.reuseMemory(order), workflow)
                      .map((_: Completed) => Right(Response.Accepted))
          }
        }

    case DetachOrder(orderId) =>
      if (shuttingDown)
        Future.failed(AgentIsShuttingDown.throwable)
      else
        orderRegister.get(orderId) match {
          case Some(orderEntry) =>
            // TODO Antwort erst nach OrderDetached _und_ Terminated senden, wenn Actor aus orderRegister entfernt worden ist
            // Bei langsamem Agenten, schnellem Controller-Wiederanlauf kann DetachOrder doppelt kommen, während OrderActor sich noch beendet.
            orderEntry.order.detaching match {
              case Left(problem) => Future.failed(problem.throwable)
              case Right(_) =>
                val promise = Promise[Unit]()
                orderEntry.detachResponses ::= promise
                (orderEntry.actor ? OrderActor.Command.HandleEvent(OrderDetached))
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
              case Right(Some(event)) =>
                // Several MarkOrder in sequence are not properly handled
                // one after the other because execution is asynchronous.
                // A second command may may see the same not yet updated order.
                // TODO Queue for each order? And no more OrderActor?
                (orderEntry.actor ? OrderActor.Command.HandleEvent(event))
                  .mapTo[Completed]
                  .map(_ => Right(AgentCommand.Response.Accepted))
            }
      }

    case GetOrder(orderId) =>
      executeCommandForOrderId(orderId, orderEntry =>
        Future.successful(GetOrder.Response(
          orderEntry.order))
      ).map((r: Response) => Right(r))

    case GetOrderIds =>
      Future.successful(Right(GetOrderIds.Response(
        orderRegister.keys)))

    case GetOrders =>
      Future.successful(Right(GetOrders.Response(
        for (orderEntry <- orderRegister.values) yield orderEntry.order)))

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

  private def startJobActors(workflow: Workflow): Unit =
    for ((jobKey, job) <- workflow.keyToJob) {
      if (job.agentPath == ownAgentPath) {
        val jobConf = JobConf(
          jobKey, job, workflow, controllerId,
          sigKillDelay = job.sigkillDelay getOrElse conf.defaultJobSigkillDelay)
        val jobActor = watch(actorOf(
          JobActor.props(jobConf, executorConf, id => persistence.currentState.pathToJobResource.checked(id)),
          uniqueActorName(encodeAsActorName("Job:" + jobKey.name))))
        jobRegister.insert(jobKey, job, jobActor)
      }
    }

  private def executeCommandForOrderId(orderId: OrderId, body: OrderEntry => Future[Response]): Future[Response] =
    orderRegister.checked(orderId) match {
      case Left(problem) =>
        Future.failed(problem.throwable)
      case Right(orderEntry) =>
        body(orderEntry)
    }

  private def attachOrder(order: Order[Order.IsFreshOrReady], workflow: Workflow): Future[Completed] = {
    val actor = newOrderActor(order, workflow)
    orderRegister.insert(order, workflow, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttachedToAgent
  }

  private def newOrderActor(order: Order[Order.State], workflow: Workflow) =
    watch(actorOf(
      OrderActor.props(order.id, workflow, journalActor = journalActor, orderActorConf, controllerId),
      name = uniqueActorName(encodeAsActorName("Order:" + order.id.string))))

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent): Unit = {
    val checkedFollowUps = orderEventHandler.handleEvent(order.id <-: event)
    orderRegister(order.id).order = order
    for (followUps <- checkedFollowUps.onProblem(p => logger.error(p))) {
      followUps foreach {
        case FollowUp.Processed(jobKey) =>
          //TODO assertThat(orderEntry.jobOption exists (_.jobPath == job.jobPath))
          for (jobEntry <- jobRegister.checked(jobKey).onProblem(p => logger.error(p withKey order.id))) {
            jobEntry.queue -= order.id
          }

        case FollowUp.AddChild(childOrder) =>
          val workflow = workflowRegister(childOrder.workflowId)
          val actor = newOrderActor(childOrder, workflow)
          orderRegister.insert(childOrder, workflow, actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder.id)

        case FollowUp.Delete(deleteOrderId) =>
          deleteOrder(deleteOrderId)

        case o: FollowUp.AddOffered =>
          sys.error(s"Unexpected FollowUp: $o")  // Only Controller handles this
      }
    }
  }

  private def proceedWithOrder(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    val order = orderEntry.order
    if (order.isAttached) {
      order.maybeDelayedUntil match {
        case Some(until) if Timestamp.now < until =>
          // TODO Schedule only the next order ?
          orderEntry.timer := alarmClock.scheduleAt(until) {
            self ! Internal.Due(orderId)
          }

        case _ =>
          val keyedEvents = orderEventSource.nextEvents(order.id)
          keyedEvents foreach {
            case KeyedEvent(orderId, OrderBroken(problem)) =>
              logger.error(s"Order ${orderId.string} is broken: $problem")

            case KeyedEvent(orderId_, event) =>
              orderRegister(orderId_).actor ? OrderActor.Command.HandleEvent(event)  // Ignore response ???
              // TODO Not awaiting the response may lead to duplicate events
              //  for example when OrderSuspensionMarked is emitted after OrderProcessed and before OrderMoved.
              //  Then, two OrderMoved are emitted, because the second event is based on the same Order state.
          }
          if (keyedEvents.isEmpty && order.isProcessable) {
            onOrderIsProcessable(orderEntry)
          }
      }
    }
  }

  private def onOrderIsProcessable(orderEntry: OrderEntry): Unit =
    if (!shuttingDown) {
      for (_ <- orderEntry.order.attached.onProblem(p => logger.error(s"onOrderIsProcessable: $p"))) {
        orderEntry.instruction match {
          case execute: Execute =>
            val checkedJobKey = execute match {
              case _: Execute.Anonymous => Right(orderEntry.workflow.anonymousJobKey(orderEntry.order.workflowPosition))
              case o: Execute.Named     => orderEntry.workflow.jobKey(orderEntry.order.position.branchPath, o.name)  // defaultArguments are extracted later
            }
            for (jobEntry <- checkedJobKey.flatMap(jobRegister.checked).onProblem(p => logger.error(p))) {
              onOrderAvailableForJob(orderEntry.order.id, jobEntry)
            }

          case _ =>
        }
      }
    }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit =
    // TODO Make this more functional!
    if (!jobEntry.queue.contains(orderId)) {
      jobEntry.queue += orderId
      if (jobEntry.waitingForOrder) {
        tryStartProcessing(jobEntry)
      } else {
        jobEntry.actor ! JobActor.Input.OrderAvailable
      }
    }

  private def tryStartProcessing(jobEntry: JobEntry): Unit =
    jobEntry.queue.dequeue() match {
      case Some(orderId) =>
        orderRegister.get(orderId) match {
          case None =>
            logger.warn(s"Unknown $orderId was enqueued for ${jobEntry.jobKey}. Order has been removed?")

          case Some(orderEntry) =>
            startProcessing(orderEntry, jobEntry)
            jobEntry.waitingForOrder = false
        }

      case None =>
        jobEntry.waitingForOrder = true
    }

  private def startProcessing(orderEntry: OrderEntry, jobEntry: JobEntry): Unit = {
    val defaultArguments = orderEntry.instruction match {
      case o: Execute.Named => o.defaultArguments
      case _ => Map.empty[String, Expression]
    }
    orderEntry.actor !
      OrderActor.Input.StartProcessing(jobEntry.actor, jobEntry.workflowJob, defaultArguments)
  }

  private def deleteOrder(orderId: OrderId): Unit =
    for (orderEntry <- orderRegister.get(orderId)) {
      orderEntry.actor ! OrderActor.Input.Terminate()
      orderRegister.remove(orderId)
    }

  override def unhandled(message: Any) =
    message match {
      case Terminated(actorRef) if jobRegister contains actorRef =>
        val jobKey = jobRegister.actorToKey(actorRef)
        if (shuttingDown) {
          logger.trace(s"Actor '$jobKey' stopped")
        } else {
          logger.error(s"Actor '$jobKey' stopped unexpectedly")
        }
        jobRegister.onActorTerminated(actorRef)
        shutdown.continue()

      case Terminated(actorRef) if orderRegister contains actorRef =>
        val orderEntry = orderRegister(actorRef)
        val orderId = orderEntry.order.id
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
    new OrderEventSource(persistence.currentState)

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
    final case class Due(orderId: OrderId)
    object StillTerminating extends DeadLetterSuppression
  }
}
