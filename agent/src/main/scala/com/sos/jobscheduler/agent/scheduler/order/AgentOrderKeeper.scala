package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{DeadLetterSuppression, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, CancelOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, KeepEvents, OrderCommand, Response}
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
import com.sos.jobscheduler.agent.scheduler.problems.AgentIsShuttingDownProblem
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.akkautils.Akkas.{encodeAsActorName, uniqueActorName}
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.filebased.FileBasedVerifier
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.{OrderBroken, OrderDetached, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.WorkflowEvent.WorkflowAttached
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import java.nio.file.Path
import java.time.ZoneId
import monix.execution.{Cancelable, Scheduler}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
  * Keeper of one Master's orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  masterId: MasterId,
  journalFileBase: Path,
  signatureVerifier: SignatureVerifier,
  newTaskRunner: TaskRunner.Factory,
  implicit private val askTimeout: Timeout,
  keyedEventBus: StampedKeyedEventBus,
  conf: AgentConfiguration)(
  implicit scheduler: Scheduler)
extends MainJournalingActor[Event] with Stash {

  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val workflowVerifier = new FileBasedVerifier(signatureVerifier, Workflow.topJsonDecoder)
  private val journalMeta = JournalMeta(SnapshotJsonFormat, AgentKeyedEventJsonCodec, journalFileBase)
  private val eventWatch = new JournalEventWatch(journalMeta, conf.config)
  protected val journalActor = watch(actorOf(
    JournalActor.props(journalMeta, conf.config, keyedEventBus, scheduler),
    "Journal"))
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val orderActorConf = OrderActor.Conf(conf.config)
  private val orderRegister = new OrderRegister
  private val orderProcessor = new OrderProcessor(workflowRegister.idToWorkflow.checked, orderRegister.idToOrder)

  private object termination {
    private var terminateCommand: Option[AgentCommand.Terminate] = None
    private var snapshotTaken = false
    private var stillTerminatingSchedule: Option[Cancelable] = None
    private var terminatingOrders = false
    private var terminatingJobs = false
    private var terminatingJournal = false

    def terminating = terminateCommand.isDefined

    def start(terminate: AgentCommand.Terminate): Unit =
      if (!terminating) {
        terminateCommand = Some(terminate)
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
      if (terminating) {
        snapshotTaken = true
        continue()
      }

    def continue() =
      for (terminate <- terminateCommand) {
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
  import termination.terminating

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    recover()
  }

  private def recover(): Unit = {
    val recoverer = new OrderJournalRecoverer(journalMeta)
    recoverer.recoverAll()
    val state = recoverer.state
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
    recoverer.startJournalAndFinishRecovery(journalActor = journalActor, orderRegister.recoveredJournalingActors, Some(eventWatch))
  }

  override def postStop() = {
    termination.close()
    eventWatch.close()
    super.postStop()
    logger.debug("Stopped")
  }

  def snapshots = Future.successful(workflowRegister.workflows)

  def receive = {
    case OrderActor.Output.RecoveryFinished(order) =>
      orderRegister(order.id).order = order
      proceedWithOrder(order.id)

    case JournalRecoverer.Output.JournalIsReady =>
      logger.info(s"${orderRegister.size} Orders and ${workflowRegister.size} Workflows recovered")
      persist(AgentMasterEvent.AgentReadyForMaster(ZoneId.systemDefault.getId)) { _ =>
        become("ready")(ready)
        unstashAll()
        logger.info("Ready")
      }

    case _: AgentCommand.Terminate =>
      logger.info("Command 'Terminate' terminates Agent while recovering")
      context.stop(self)
      sender() ! AgentCommand.Response.Accepted

    case _ =>
      stash()
  }

  private def ready: Receive = {
    case Input.ExternalCommand(cmd, response) =>
      response.completeWith(processOrderCommand(cmd))

    case Input.GetEventWatch =>
      sender() ! eventWatch

    case terminate: AgentCommand.Terminate =>
      termination.start(terminate)
      sender() ! AgentCommand.Response.Accepted

    case JournalActor.Output.SnapshotTaken =>
      termination.onSnapshotTaken()

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id =>
      if (!terminating) {
        handleOrderEvent(order, event)
        (event, orderRegister(order.id).instruction) match {
          case (_: OrderStarted, _: Execute) =>  // Special for OrderActor: it issues immediately an OrderProcessingStarted
          case _ =>
            proceedWithOrder(order.id)
        }
      }

    case JobActor.Output.ReadyForOrder if jobRegister contains sender() =>
      if (!terminating) {
        tryStartProcessing(jobRegister(sender()))
      }

    case Internal.ContinueAttachOrder(order, workflow, promise) =>
      promise completeWith {
        if (!workflow.isDefinedAt(order.position))
          Future.successful(Invalid(Problem.pure(s"Unknown Position ${order.workflowPosition}")))
        else if (orderRegister contains order.id) {
          // May occur after Master restart when Master is not sure about order has been attached previously.
          logger.debug(s"Ignoring duplicate AttachOrder(${workflow.id})")
          Future.successful(Valid(Response.Accepted))
        } else if (terminating)
          Future.successful(Invalid(AgentIsShuttingDownProblem))
        else
          attachOrder(workflowRegister.reuseMemory(order), workflow)
            .map((_: Completed) => Valid(Response.Accepted))
      }

    case Internal.Due(orderId) if orderRegister contains orderId =>
      proceedWithOrder(orderId)
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Checked[Response]] = cmd match {
    case AttachOrder(order, signedWorkflowString) if !terminating =>
      order.attached match {
        case Invalid(problem) => Future.successful(Invalid(problem))
        case Valid(agentRefPath) =>
          workflowVerifier.verify(signedWorkflowString) match {
            case Invalid(problem) => Future.successful(Invalid(problem))
            case Valid(verified) =>
              val workflow = verified.signedFileBased.value.reduceForAgent(agentRefPath)
              (workflowRegister.get(order.workflowId) match {
                case None =>
                  logger.info(verified.toString)
                  persist(WorkflowAttached(workflow)) { stampedEvent =>
                    workflowRegister.handleEvent(stampedEvent.value)
                    startJobActors(workflow)
                    Valid(workflow)
                  }
                case Some(registeredWorkflow) if registeredWorkflow.withoutSource == workflow.withoutSource =>
                  Future.successful(Valid(registeredWorkflow))
                case Some(_) =>
                  Future.successful(Invalid(Problem.pure(s"Changed ${order.workflowId}")))
              })
              .flatMap {
                case Invalid(problem) => Future.successful(Invalid(problem))
                case Valid(registeredWorkflow) =>
                  // Reuse registeredWorkflow to reduce memory usage!
                  promiseFuture[Checked[AgentCommand.Response.Accepted]] { promise =>
                    self ! Internal.ContinueAttachOrder(order, registeredWorkflow, promise)
                  }
              }
          }
      }

    case DetachOrder(orderId) if !terminating =>
      orderRegister.get(orderId) match {
        case Some(orderEntry) =>
          // TODO Antwort erst nach OrderDetached _und_ Terminated senden, wenn Actor aus orderRegister entfernt worden ist
          // Bei langsamem Agenten, schnellem Master-Wiederanlauf kann DetachOrder doppelt kommen, während OrderActor sich noch beendet.
          orderEntry.order.detaching match {
            case Invalid(problem) => Future.failed(problem.throwable)
            case Valid(_) =>
              orderEntry.detaching = true  // OrderActor is isTerminating
              (orderEntry.actor ? OrderActor.Command.HandleEvent(OrderDetached))
                .mapTo[Completed]
                .map(_ => Valid(AgentCommand.Response.Accepted))
          }
        case None =>
          // May occur after Master restart when Master is not sure about order has been detached previously.
          logger.debug(s"Ignoring duplicate $cmd")
          Future.successful(Valid(AgentCommand.Response.Accepted))
      }

    case CancelOrder(orderId, mode) =>
      orderRegister.checked(orderId) match {
        case Invalid(problem) =>
          Future.failed(problem.throwable)
        case Valid(orderEntry) =>
          if (orderEntry.detaching)
            Future.successful(Valid(AgentCommand.Response.Accepted))
          else
            orderProcessor.cancel(orderId, mode, isAgent = true) match {
              case Invalid(problem) => Future.failed(problem.throwable)
              case Valid(None) => Future.successful(Valid(AgentCommand.Response.Accepted))
              case Valid(Some(event)) =>
                (orderEntry.actor ? OrderActor.Command.HandleEvent(event)).mapTo[Completed] map { _ => Valid(AgentCommand.Response.Accepted) }
            }
      }

    case GetOrder(orderId) =>
      executeCommandForOrderId(orderId) { orderEntry =>
        Future.successful(GetOrder.Response(
          orderEntry.order))
      } map ((r: Response) => Valid(r))

    case GetOrderIds =>
      Future.successful(Valid(GetOrderIds.Response(
        orderRegister.keys)))

    case GetOrders =>
      Future.successful(Valid(GetOrders.Response(
        for (orderEntry <- orderRegister.values) yield orderEntry.order)))

    case KeepEvents(eventId) if !terminating =>
      Future {  // Concurrently!
        eventWatch.keepEvents(eventId).orThrow
        Valid(AgentCommand.Response.Accepted)
      }

    case _ if terminating =>
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
      case Invalid(problem) =>
        Future.failed(problem.throwable)
      case Valid(orderEntry) =>
        body(orderEntry)
    }

  private def attachOrder(order: Order[Order.FreshOrReady], workflow: Workflow): Future[Completed] = {
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
          //TODO assert(orderEntry.jobOption exists (_.jobPath == job.jobPath))
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
        case Some(until) if now < until =>
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
              if (order.isState[Order.FreshOrReady]) {
                onOrderFreshOrReady(orderEntry)
              }
          }
      }
    }
  }

  private def onOrderFreshOrReady(orderEntry: OrderEntry): Unit =
    if (!terminating) {
      for (_ <- orderEntry.order.attached onProblem (p => logger.error(s"onOrderFreshOrReady: $p"))) {
        orderEntry.instruction match {
          case execute: Execute =>
            val checkedJobKey = execute match {
              case _: Execute.Anonymous => Valid(orderEntry.workflow.anonymousJobKey(orderEntry.order.workflowPosition))
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
    logger.debug(s"$orderId is queuing for ${jobEntry.jobKey}")
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
    logger.debug(s"${orderEntry.order.id} is going to be processed by ${jobEntry.jobKey}")
    val defaultArguments = orderEntry.instruction match {
      case o: Execute.Named => o.defaultArguments
      case _ => Map.empty[String, String]
    }
    //assert(job.jobPath == jobEntry.jobPath)
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
        if (terminating) {
          logger.debug(s"Actor '$jobKey' stopped")
        } else {
          logger.error(s"Actor '$jobKey' stopped unexpectedly")
        }
        jobRegister.onActorTerminated(actorRef)
        termination.continue()

      case Terminated(actorRef) if orderRegister contains actorRef =>
        val orderId = orderRegister(actorRef).order.id
        logger.debug(s"Actor '$orderId' stopped")
        orderRegister.onActorTerminated(actorRef)
        termination.continue()

      case Terminated(`journalActor`) if terminating =>
        context.stop(self)

      case Internal.StillTerminating =>
        termination.onStillTerminating()

      case _ =>
        super.unhandled(message)
    }

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[Workflow],
    Subtype[Order[Order.State]])

  sealed trait Input
  object Input {
    final case object GetEventWatch
    final case class ExternalCommand(command: OrderCommand, response: Promise[Checked[Response]])
  }

  private object Internal {
    final case class ContinueAttachOrder(order: Order[Order.FreshOrReady], workflow: Workflow, promise: Promise[Checked[AgentCommand.Response.Accepted]])
    final case class Due(orderId: OrderId)
    object StillTerminating extends DeadLetterSuppression
  }
}
