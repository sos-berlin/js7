package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, DeadLetterSuppression, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.AgentState
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, CancelOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand, ReleaseEvents, Response}
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent.AgentReadyForMaster
import com.sos.jobscheduler.agent.data.problems.AgentDuplicateOrderProblem
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
import com.sos.jobscheduler.agent.scheduler.problems.AgentIsShuttingDownProblem
import com.sos.jobscheduler.base.crypt.SignatureVerifier
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.utils.SetOnce
import com.sos.jobscheduler.common.akkautils.Akkas.{encodeAsActorName, uniqueActorName}
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.core.problems.ReverseReleaseEventsProblem
import com.sos.jobscheduler.data.crypt.FileBasedVerifier
import com.sos.jobscheduler.data.event.JournalEvent.JournalEventsReleased
import com.sos.jobscheduler.data.event.{<-:, Event, EventId, JournalState, JournaledState, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.execution.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.data.execution.workflow.OrderProcessor
import com.sos.jobscheduler.data.execution.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderBroken, OrderDetached}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.WorkflowEvent.WorkflowAttached
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import java.time.ZoneId
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import shapeless.tag

/**
  * Keeper of one Master's orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  masterId: MasterId,
  recovered_ : OrderJournalRecoverer.Recovered,
  signatureVerifier: SignatureVerifier,
  newTaskRunner: TaskRunner.Factory,
  persistence: JournaledStatePersistence[AgentState],
  implicit private val askTimeout: Timeout,
  conf: AgentConfiguration)(
  implicit scheduler: Scheduler)
extends MainJournalingActor[AgentState, Event]
with Stash {

  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor = tag[JournalActor.type](persistence.journalActor: ActorRef)

  private var journalState = JournalState.empty
  private val workflowVerifier = new FileBasedVerifier(signatureVerifier, Workflow.topJsonDecoder)
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val orderActorConf = OrderActor.Conf(conf.config)
  private val orderRegister = new OrderRegister
  private val orderProcessor = new OrderProcessor(workflowRegister.idToWorkflow.checked, orderRegister.idToOrder)

  private object shutdown {
    private var shutDownCommand: Option[AgentCommand.ShutDown] = None
    private var snapshotTaken = false
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
        journalActor ! JournalActor.Input.TakeSnapshot  // Take snapshot before OrderActors are stopped
        stillTerminatingSchedule = Some(scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
          self ! Internal.StillTerminating
        })
        continue()
      }

    def close() = {
      stillTerminatingSchedule foreach (_.cancel())
    }

    def onStillTerminating() =
      logger.info(s"Still terminating, waiting for ${orderRegister.size} orders, ${jobRegister.size} jobs" +
        (if (!snapshotTaken) ", and the snapshot" else ""))

    def onSnapshotTaken(): Unit =
      if (shuttingDown) {
        snapshotTaken = true
        continue()
      }

    def continue() =
      for (terminate <- shutDownCommand) {
        logger.trace(s"termination.continue: ${orderRegister.size} orders, ${jobRegister.size} jobs ${if (snapshotTaken) ", snapshot taken" else ""}")
        if (snapshotTaken) {
          if (!terminatingOrders) {
            terminatingOrders = true
            for (o <- orderRegister.values if !o.detaching) {
              o.actor ! OrderActor.Input.Terminate(terminate.sigtermProcesses, terminate.sigkillProcessesAfter)
            }
          }
          if (orderRegister.isEmpty) {
            if (!terminatingJobs) {
              terminatingJobs = true
              for (a <- jobRegister.values) a.actor ! JobActor.Input.Terminate()
            }
            if (jobRegister.isEmpty && !terminatingJournal) {
              terminatingJournal = true
              journalActor ! JournalActor.Input.Terminate
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
    shutdown.close()
    super.postStop()
    logger.debug("Stopped" + shutdown.since.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))
  }

  def snapshots = Future.successful(workflowRegister.workflows)

  def receive = {
    case Internal.Recover(recovered) =>
      recover(recovered)
      become("Recovering")(recovering)
      unstashAll()
    case _ => stash()
  }

  private def recover(recovered: OrderJournalRecoverer.Recovered): Unit = {
    val state = recovered.agentState
    journalState = state.journalState
    for (workflow <- state.idToWorkflow.values)
      wrapException(s"Error when recovering ${workflow.path}") {
        workflowRegister.recover(workflow)
        startJobActors(workflow)
      }
    for (recoveredOrder <- state.idToOrder.values)
      wrapException(s"Error when recovering ${recoveredOrder.id}") {
        val order = workflowRegister.reuseMemory(recoveredOrder)
        val workflow = workflowRegister(order.workflowId)  // Workflow is expected to be recovered
        val actor = newOrderActor(order)
        orderRegister.recover(order, workflow, actor)
        actor ! OrderActor.Input.Recover(order)
      }
    val agentState = AgentState(recovered.eventId,
      JournaledState.Standards.empty.copy(journalState = journalState),
      orderRegister.values.view.map(o => o.order.id -> o.order).toMap,
      workflowRegister.workflows.view.map(o => o.id -> o).toMap)
    persistence.start(agentState)
    recovered.startJournalAndFinishRecovery(
      journalActor = journalActor,
      agentState,
      orderRegister.recoveredJournalingActors)
    become("Recovering")(recovering)
  }

  private def recovering: Receive = {
    case OrderActor.Output.RecoveryFinished(order) =>
      orderRegister(order.id).order = order
      proceedWithOrder(order.id)

    case JournalRecoverer.Output.JournalIsReady(journalHeader) =>
      logger.info(s"${orderRegister.size} Orders and ${workflowRegister.size} Workflows recovered")
      if (!journalState.userIdToReleasedEventId.contains(masterId.toUserId)) {
        // Automatically add Master's UserId to list of users allowed to release events,
        // to avoid deletion of journal files due to an empty list, becore master has read the events.
        // The master has to send ReleaseEvents commands to release obsolete journal files.
        persist(JournalEventsReleased(masterId.toUserId, EventId.BeforeFirst)) {
          case (Stamped(_,_, _ <-: event), journaledState) =>
            journalState = journalState.applyEvent(event)
        }
      }
      persist(AgentReadyForMaster(ZoneId.systemDefault.getId, totalRunningTime = journalHeader.totalRunningTime)) { (_, _) =>
        become("ready")(ready)
        unstashAll()
        logger.info("Ready")
      }

    case _: AgentCommand.ShutDown =>
      logger.info("Command 'ShutDown' terminates Agent while recovering")
      context.stop(self)
      sender() ! AgentCommand.Response.Accepted

    case _ =>
      stash()
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

    case Internal.ContinueAttachOrder(order, workflow, promise) =>
      promise completeWith {
        if (!workflow.isDefinedAt(order.position))
          Future.successful(Left(Problem.pure(s"Unknown Position ${order.workflowPosition}")))
        else if (orderRegister contains order.id)
          Future.successful(Left(AgentDuplicateOrderProblem(order.id)))
        else if (shuttingDown)
          Future.successful(Left(AgentIsShuttingDownProblem))
        else
          attachOrder(workflowRegister.reuseMemory(order), workflow)
            .map((_: Completed) => Right(Response.Accepted))
      }

    case Internal.Due(orderId) if orderRegister contains orderId =>
      proceedWithOrder(orderId)
  }

  private def processCommand(cmd: AgentCommand): Future[Checked[Response]] = cmd match {
    case cmd: OrderCommand => processOrderCommand(cmd)

    case AgentCommand.TakeSnapshot =>
      (journalActor ? JournalActor.Input.TakeSnapshot)
        .mapTo[JournalActor.Output.SnapshotTaken.type]
        .map(_ => Right(AgentCommand.Response.Accepted))

    case _ => Future.successful(Left(Problem(s"Unknown command: ${cmd.getClass.simpleScalaName}")))  // Should not happen
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Checked[Response]] = cmd match {
    case AttachOrder(order, signedWorkflowString) if !shuttingDown =>
      order.attached match {
        case Left(problem) => Future.successful(Left(problem))
        case Right(agentRefPath) =>
          workflowVerifier.verify(signedWorkflowString) match {
            case Left(problem) => Future.successful(Left(problem))
            case Right(verified) =>
              if (orderRegister contains order.id)
                Future.successful(Left(AgentDuplicateOrderProblem(order.id)))
              else {
                val workflow = verified.signedFileBased.value.reduceForAgent(agentRefPath)
                (workflowRegister.get(order.workflowId) match {
                  case None =>
                    logger.info(verified.toString)
                    persist(WorkflowAttached(workflow)) { (stampedEvent, journaledState) =>
                      workflowRegister.handleEvent(stampedEvent.value)
                      startJobActors(workflow)
                      Right(workflow)
                    }
                  case Some(registeredWorkflow) if registeredWorkflow.withoutSource == workflow.withoutSource =>
                    Future.successful(Right(registeredWorkflow))
                  case Some(_) =>
                    Future.successful(Left(Problem.pure(s"Changed ${order.workflowId}")))
                })
                .flatMap {
                  case Left(problem) => Future.successful(Left(problem))
                  case Right(registeredWorkflow) =>
                    // Reuse registeredWorkflow to reduce memory usage!
                    promiseFuture[Checked[AgentCommand.Response.Accepted]] { promise =>
                      self ! Internal.ContinueAttachOrder(order, registeredWorkflow, promise)
                    }
                }
              }
          }
      }

    case DetachOrder(orderId) if !shuttingDown =>
      orderRegister.get(orderId) match {
        case Some(orderEntry) =>
          // TODO Antwort erst nach OrderDetached _und_ Terminated senden, wenn Actor aus orderRegister entfernt worden ist
          // Bei langsamem Agenten, schnellem Master-Wiederanlauf kann DetachOrder doppelt kommen, während OrderActor sich noch beendet.
          orderEntry.order.detaching match {
            case Left(problem) => Future.failed(problem.throwable)
            case Right(_) =>
              orderEntry.detaching = true  // OrderActor is isTerminating
              (orderEntry.actor ? OrderActor.Command.HandleEvent(OrderDetached))
                .mapTo[Completed]
                .map(_ => Right(AgentCommand.Response.Accepted))
          }
        case None =>
          // May occur after Master restart when Master is not sure about order has been detached previously.
          logger.debug(s"Ignoring duplicate $cmd")
          Future.successful(Right(AgentCommand.Response.Accepted))
      }

    case CancelOrder(orderId, mode) =>
      orderRegister.checked(orderId) match {
        case Left(problem) =>
          Future.failed(problem.throwable)
        case Right(orderEntry) =>
          if (orderEntry.detaching)
            Future.successful(Right(AgentCommand.Response.Accepted))
          else
            orderProcessor.cancel(orderId, mode, isAgent = true) match {
              case Left(problem) => Future.failed(problem.throwable)
              case Right(None) => Future.successful(Right(AgentCommand.Response.Accepted))
              case Right(Some(event)) =>
                (orderEntry.actor ? OrderActor.Command.HandleEvent(event)).mapTo[Completed] map { _ => Right(AgentCommand.Response.Accepted) }
            }
      }

    case GetOrder(orderId) =>
      executeCommandForOrderId(orderId) { orderEntry =>
        Future.successful(GetOrder.Response(
          orderEntry.order))
      } map ((r: Response) => Right(r))

    case GetOrderIds =>
      Future.successful(Right(GetOrderIds.Response(
        orderRegister.keys)))

    case GetOrders =>
      Future.successful(Right(GetOrders.Response(
        for (orderEntry <- orderRegister.values) yield orderEntry.order)))

    case ReleaseEvents(after) if !shuttingDown =>
      val userId = masterId.toUserId
      val current = journalState.userIdToReleasedEventId(userId)  // Must contain userId
      if (after < current)
        Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = after, currentUntilEventId = current)))
      else
        persist(JournalEventsReleased(userId, after)) {
          case (Stamped(_,_, _ <-: event), journaledState) =>
            journalState = journalState.applyEvent(event)
            Right(AgentCommand.Response.Accepted)
          }

    case _ if shuttingDown =>
      Future.failed(AgentIsShuttingDownProblem.throwable)
  }

  private def startJobActors(workflow: Workflow): Unit =
    for ((jobKey, job) <- workflow.keyToJob) {
      val jobActor = watch(actorOf(
        JobActor.props(JobActor.Conf(jobKey, job, newTaskRunner, temporaryDirectory = conf.temporaryDirectory,
          executablesDirectory = conf.executableDirectory, scriptInjectionAllowed = conf.scriptInjectionAllowed))
        /*TODO name actor?*/))
      jobRegister.insert(jobKey, jobActor)
    }

  private def executeCommandForOrderId(orderId: OrderId)(body: OrderEntry => Future[Response]): Future[Response] =
    orderRegister.checked(orderId) match {
      case Left(problem) =>
        Future.failed(problem.throwable)
      case Right(orderEntry) =>
        body(orderEntry)
    }

  private def attachOrder(order: Order[Order.IsFreshOrReady], workflow: Workflow): Future[Completed] = {
    val actor = newOrderActor(order)
    orderRegister.insert(order, workflow, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttached
  }

  private def newOrderActor(order: Order[Order.State]) =
    watch(actorOf(
      OrderActor.props(order.id, journalActor = journalActor, orderActorConf),
      name = uniqueActorName(encodeAsActorName("Order-" + order.id.string))))

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent): Unit = {
    val checkedFollowUps = orderProcessor.handleEvent(order.id <-: event)
    orderRegister(order.id).order = order
    for (followUps <- checkedFollowUps onProblem (p => logger.error(p))) {
      followUps foreach {
        case FollowUp.Processed(jobKey) =>
          //TODO assertThat(orderEntry.jobOption exists (_.jobPath == job.jobPath))
          for (jobEntry <- jobRegister.checked(jobKey) onProblem (p => logger.error(p withKey order.id))) {
            jobEntry.queue -= order.id
          }

        case FollowUp.AddChild(childOrder) =>
          val actor = newOrderActor(childOrder)
          orderRegister.insert(childOrder, workflowRegister(childOrder.workflowId), actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder.id)

        case FollowUp.Remove(removeOrderId) =>
          removeOrder(removeOrderId)

        case o: FollowUp.AddOffered =>
          sys.error(s"Unexpected FollowUp: $o")  // Only Master handles this
      }
    }
  }

  private def proceedWithOrder(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    val order = orderEntry.order
    if (order.isAttached) {
      order.state.maybeDelayedUntil match {
        case Some(until) if Timestamp.now < until =>
          orderEntry.at(until) {  // TODO Schedule only the next order ?
            self ! Internal.Due(orderId)
          }
        case _ =>
          orderProcessor.nextEvent(order.id) match {
            case Some(KeyedEvent(orderId, OrderBroken(problem))) =>
              logger.error(s"Order ${orderId.string} is broken: $problem")

            case Some(KeyedEvent(orderId_, event)) =>
              orderRegister(orderId_).actor ? OrderActor.Command.HandleEvent(event)  // Ignore response ???

            case None =>
              if (order.isState[Order.IsFreshOrReady]) {
                onOrderFreshOrReady(orderEntry)
              }
          }
      }
    }
  }

  private def onOrderFreshOrReady(orderEntry: OrderEntry): Unit =
    if (!shuttingDown) {
      for (_ <- orderEntry.order.attached onProblem (p => logger.error(s"onOrderFreshOrReady: $p"))) {
        orderEntry.instruction match {
          case execute: Execute =>
            val checkedJobKey = execute match {
              case _: Execute.Anonymous => Right(orderEntry.workflow.anonymousJobKey(orderEntry.order.workflowPosition))
              case o: Execute.Named     => orderEntry.workflow.jobKey(orderEntry.order.position.branchPath, o.name)  // defaultArguments are extracted later
            }
            for (jobEntry <- checkedJobKey flatMap jobRegister.checked onProblem (p => logger.error(p))){
              onOrderAvailableForJob(orderEntry.order.id, jobEntry)
            }

          case _ =>
        }
      }
    }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit = {
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
            for (workflowJob <- orderEntry.checkedJob onProblem (o => logger.error(o))) {
              startProcessing(orderEntry, jobEntry.jobKey, workflowJob, jobEntry)
            }
            jobEntry.waitingForOrder = false
        }

      case None =>
        jobEntry.waitingForOrder = true
    }

  private def startProcessing(orderEntry: OrderEntry, jobKey: JobKey, job: WorkflowJob, jobEntry: JobEntry): Unit = {
    val defaultArguments = orderEntry.instruction match {
      case o: Execute.Named => o.defaultArguments
      case _ => Map.empty[String, String]
    }
    //assertThat(job.jobPath == jobEntry.jobPath)
    jobEntry.waitingForOrder = false
    orderEntry.actor ! OrderActor.Input.StartProcessing(jobKey, job, jobEntry.actor, defaultArguments)
  }

  private def removeOrder(orderId: OrderId): Unit = {
    for (orderEntry <- orderRegister.get(orderId)) {
      orderEntry.actor ! OrderActor.Input.Terminate()
      //context.unwatch(orderEntry.actor)
      orderRegister.remove(orderId)
    }
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
        val orderId = orderRegister(actorRef).order.id
        logger.debug(s"Actor '$orderId' stopped")
        orderRegister.onActorTerminated(actorRef)
        shutdown.continue()

      case Terminated(`journalActor`) if shuttingDown =>
        context.stop(self)

      case Internal.StillTerminating =>
        shutdown.onStillTerminating()

      case _ =>
        super.unhandled(message)
    }

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    final case class ExternalCommand(command: AgentCommand, response: Promise[Checked[Response]])
  }

  private object Internal {
    final case class Recover(recovered: OrderJournalRecoverer.Recovered)
    final case class ContinueAttachOrder(order: Order[Order.IsFreshOrReady], workflow: Workflow, promise: Promise[Checked[AgentCommand.Response.Accepted]])
    final case class Due(orderId: OrderId)
    object StillTerminating extends DeadLetterSuppression
  }
}
