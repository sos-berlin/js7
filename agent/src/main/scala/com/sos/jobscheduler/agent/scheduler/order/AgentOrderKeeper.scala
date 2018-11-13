package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{DeadLetterSuppression, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, AttachOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, KeepEvents, OrderCommand, Response}
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
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.akkautils.Akkas.{encodeAsActorName, uniqueActorName}
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.WorkflowEvent.WorkflowAttached
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.typesafe.config.Config
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
  executableDirectory: Path,
  newTaskRunner: TaskRunner.Factory,
  implicit private val askTimeout: Timeout,
  keyedEventBus: StampedKeyedEventBus,
  config: Config,
  implicit private val timerService: TimerService)(
  implicit scheduler: Scheduler)
extends MainJournalingActor[Event] with Stash {

  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val journalMeta = JournalMeta(SnapshotJsonFormat, AgentKeyedEventJsonCodec, journalFileBase)
  private val eventWatch = new JournalEventWatch(journalMeta, config)
  protected val journalActor = watch(actorOf(
    JournalActor.props(journalMeta, config, keyedEventBus, scheduler),
    "Journal"))
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val orderRegister = new OrderRegister(timerService)
  private val orderProcessor = new OrderProcessor(workflowRegister.idToWorkflow.checked, orderRegister.idToOrder)

  private object termination {
    private var _terminating = false
    private var snapshotTaken = false
    private var stillTerminatingSchedule: Option[Cancelable] = None
    private var terminatingOrders = false
    private var terminatingJournal = false

    def terminating = _terminating

    def start(terminate: AgentCommand.Terminate): Unit =
      if (!_terminating) {
        _terminating = true
        journalActor ! JournalActor.Input.TakeSnapshot  // Take snapshot before OrderActors are stopped
        for (a ← jobRegister.values) a.actor ! terminate
        stillTerminatingSchedule = Some(scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
          self ! Internal.StillTerminating
        })
        continue()
      }

    def close() = {
      stillTerminatingSchedule foreach (_.cancel())
    }

    def onStillTerminating() =
      logger.info(s"Still terminating, waiting for ${orderRegister.size} orders, ${jobRegister.size} jobs")

    def onSnapshotTaken(): Unit =
      if (_terminating) {
        snapshotTaken = true
        continue()
      }

    def continue() =
      if (_terminating) {
        logger.trace(s"termination.continue: ${orderRegister.size} orders, ${jobRegister.size} jobs")
        if (snapshotTaken && jobRegister.isEmpty) {
          if (orderRegister.nonEmpty && !terminatingOrders) {
            terminatingOrders = true
            for (o ← orderRegister.values if !o.detaching) {
              o.actor ! OrderActor.Input.Terminate
            }
          } else
          if (orderRegister.isEmpty && !terminatingJournal) {
            terminatingJournal = true
            journalActor ! JournalActor.Input.Terminate
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
    val recoverer = new OrderJournalRecoverer(journalMeta)(askTimeout)
    recoverer.recoverAll()
    val state = recoverer.state
    for (workflow ← state.idToWorkflow.values)
      wrapException(s"Error when recovering ${workflow.path}") {
        workflowRegister.recover(workflow)
        startJobActors(workflow)
      }
    for (recoveredOrder ← state.idToOrder.values)
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
    case OrderActor.Output.RecoveryFinished(order) ⇒
      orderRegister(order.id).order = order
      proceedWithOrder(order.id)

    case JournalRecoverer.Output.JournalIsReady ⇒
      logger.info(s"${workflowRegister.size} Workflows and ${orderRegister.size} Orders recovered")
      persist(AgentMasterEvent.AgentReadyForMaster(ZoneId.systemDefault)) { _ ⇒
        become("ready")(ready)
        unstashAll()
        logger.info("Ready")
      }

    case _: AgentCommand.Terminate ⇒
      logger.info("Command 'Terminate' terminates Agent while recovering")
      context.stop(self)
      sender() ! AgentCommand.Accepted

    case _ ⇒
      stash()
  }

  private def ready: Receive = {
    case Input.ExternalCommand(cmd, response) ⇒
      response.completeWith(processOrderCommand(cmd))

    case Input.GetEventWatch ⇒
      sender() ! eventWatch

    case terminate: AgentCommand.Terminate ⇒
      termination.start(terminate)
      sender() ! AgentCommand.Accepted

    case JournalActor.Output.SnapshotTaken ⇒
      termination.onSnapshotTaken()

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      if (!terminating) {
        handleOrderEvent(order, event)
        proceedWithOrder(order.id)
      }

    case JobActor.Output.ReadyForOrder if jobRegister contains sender() ⇒
      if (!terminating) {
        tryStartProcessing(jobRegister(sender()))
      }

    case Internal.ContinueAttachOrder(cmd @ AttachOrder(order, workflow), promise) ⇒
      promise completeWith {
        if (!workflow.isDefinedAt(order.position))
          Future.failed(Problem.eager(s"Unknown Position ${order.workflowPosition}").throwable)
        else if (orderRegister contains order.id) {
          // May occur after Master restart when Master is not sure about order has been attached previously.
          logger.debug(s"Ignoring duplicate $cmd")
          Future.successful(Accepted)
        } else if (terminating)
          Future.failed(AgentIsShuttingDownProblem.throwable)
        else
          attachOrder(workflowRegister.reuseMemory(order), workflow) map { case Completed ⇒ Accepted }
      }

    case Internal.Due(orderId) if orderRegister contains orderId ⇒
      onOrderAvailable(orderRegister(orderId))
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Response] = cmd match {
    case cmd @ AttachOrder(order, workflow) if !terminating ⇒
      order.attachedToAgent match {
        case Invalid(problem) ⇒ Future.failed(problem.throwable)
        case Valid(_) ⇒
          (workflowRegister.get(order.workflowId) match {
            case None ⇒
              persist(WorkflowAttached(workflow)) { stampedEvent ⇒
                workflowRegister.handleEvent(stampedEvent.value)
                startJobActors(workflow)
                Completed
              }
            case Some(w) if w.withoutSource == workflow.withoutSource ⇒
              Future.successful(Completed)
            case Some(_) ⇒
              Future.failed(new IllegalStateException(s"Changed ${order.workflowId}"))
          })
          .flatMap { _: Completed ⇒
            promiseFuture[Accepted] { promise ⇒
              self ! Internal.ContinueAttachOrder(cmd, promise)
            }
          }
      }

    case DetachOrder(orderId) if !terminating ⇒
      orderRegister.get(orderId) match {
        case Some(orderEntry) ⇒
          // TODO Antwort erst nach OrderDetached _und_ Terminated senden, wenn Actor aus orderRegister entfernt worden ist
          // Bei langsamem Agenten, schnellem Master-Wiederanlauf kann DetachOrder doppelt kommen, während OrderActor sich noch beendet.
          orderEntry.order.detachableFromAgent match {
            case Invalid(problem) ⇒ Future.failed(problem.throwable)
            case Valid(_) ⇒
              orderEntry.detaching = true  // OrderActor is isTerminating
              (orderEntry.actor ? OrderActor.Command.Detach).mapTo[Completed] map { _ ⇒ Accepted }  // TODO AskTimeoutException when Journal blocks
          }
        case None ⇒
          // May occur after Master restart when Master is not sure about order has been detached previously.
          logger.debug(s"Ignoring duplicate $cmd")
          Future.successful(Accepted)
      }

    case GetOrder(orderId) ⇒
      executeCommandForOrderId(orderId) { orderEntry ⇒
        Future.successful(GetOrder.Response(
          orderEntry.order))
      }

    case GetOrderIds ⇒
      Future.successful(GetOrderIds.Response(
        orderRegister.keys))

    case GetOrders ⇒
      Future.successful(GetOrders.Response(
        for (orderEntry ← orderRegister.values) yield orderEntry.order))

    case KeepEvents(eventId) if !terminating ⇒
      Future {
        eventWatch.keepEvents(eventId).orThrow
        AgentCommand.Accepted
      }

    case _ if terminating ⇒
      Future.failed(AgentIsShuttingDownProblem.throwable)
  }

  private def startJobActors(workflow: Workflow): Unit =
    for ((jobKey, job) ← workflow.keyToJob) {
      val jobActor = watch(actorOf(
        JobActor.props(jobKey, job, newTaskRunner, executableDirectory = executableDirectory)
        /*TODO name actor?*/))
      jobRegister.insert(jobKey, jobActor)
    }

  private def executeCommandForOrderId(orderId: OrderId)(body: OrderEntry ⇒ Future[Response]): Future[Response] =
    orderRegister.checked(orderId) match {
      case Invalid(problem) ⇒
        Future.failed(problem.throwable)
      case Valid(orderEntry) ⇒
        body(orderEntry)
    }

  private def attachOrder(order: Order[Order.Idle], workflow: Workflow): Future[Completed] = {
    val actor = newOrderActor(order)
    orderRegister.insert(order, workflow, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]  // TODO ask will time-out when Journal blocks
    // Now expecting OrderEvent.OrderAttached
  }

  private def newOrderActor(order: Order[Order.State]) =
    watch(actorOf(
      OrderActor.props(order.id, journalActor = journalActor, config),
      name = uniqueActorName(encodeAsActorName("Order-" + order.id.string))))

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent): Unit = {
    val orderEntry = orderRegister(order.id)
    val checkedFollowUps = orderProcessor.handleEvent(order.id <-: event)
    for (followUps ← checkedFollowUps onProblem (p ⇒ logger.error(p))) {
      followUps foreach {
        case FollowUp.Processed(jobKey) ⇒
          //TODO assert(orderEntry.jobOption exists (_.jobPath == job.jobPath))
          for (jobEntry ← jobRegister.checked(jobKey) onProblem (p ⇒ logger.error(p withKey order.id))) {
            jobEntry.queue -= order.id
          }

        case FollowUp.AddChild(childOrder) ⇒
          val actor = newOrderActor(childOrder)
          orderRegister.insert(childOrder, workflowRegister(childOrder.workflowId), actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder.id)

        case FollowUp.Remove(removeOrderId) ⇒
          removeOrder(removeOrderId)

        case o: FollowUp.AddOffered ⇒
          sys.error(s"Unexpected FollowUp: $o")  // Only Master handles this
      }
    }
    orderEntry.order = order
  }

  private def proceedWithOrder(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    val order = orderEntry.order
    if (order.isAttachedToAgent) {
      order.state match {
        case Order.Fresh(Some(scheduledAt)) if now < scheduledAt ⇒
          orderEntry.at(scheduledAt) {  // TODO Register only the next order in TimerService ?
            self ! Internal.Due(orderId)
          }

        case _: Order.Idle ⇒
          onOrderAvailable(orderEntry)

        case _ ⇒
          tryExecuteInstruction(order, orderEntry.workflow)
      }
    }
  }

  private def onOrderAvailable(orderEntry: OrderEntry): Unit =
    if (!terminating) {
      for (_ ← orderEntry.order.attachedToAgent onProblem (p ⇒ logger.error(s"onOrderAvailable: $p"))) {
        orderEntry.instruction match {
          case _: Execute ⇒
            val checkedJobKey = (orderEntry.instruction: @unchecked) match {
              case _: Execute.Anonymous ⇒ Valid(JobKey.Anonymous(orderEntry.order.workflowPosition))
              case o: Execute.Named     ⇒ orderEntry.workflow.jobKey(orderEntry.order.position.branchPath, o.name)
            }
            for (jobEntry ← checkedJobKey flatMap jobRegister.checked onProblem (p ⇒ logger.error(p))){
              onOrderAvailableForJob(orderEntry.order.id, jobEntry)
            }

          case _ ⇒
            tryExecuteInstruction(orderEntry.order, orderEntry.workflow)
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
      case Some(orderId) ⇒
        orderRegister.get(orderId) match {
          case None ⇒
            logger.warn(s"Unknown $orderId was enqueued for ${jobEntry.jobKey}. Order has been removed?")

          case Some(orderEntry) ⇒
            for (workflowJob ← orderEntry.checkedJob onProblem (o ⇒ logger.error(o))) {
              startProcessing(orderEntry, jobEntry.jobKey, workflowJob, jobEntry)
            }
            jobEntry.waitingForOrder = false
        }

      case None ⇒
        jobEntry.waitingForOrder = true
    }

  private def startProcessing(orderEntry: OrderEntry, jobKey: JobKey, job: WorkflowJob, jobEntry: JobEntry): Unit = {
    logger.debug(s"${orderEntry.order.id} is going to be processed by ${jobEntry.jobKey}")
    //assert(job.jobPath == jobEntry.jobPath)
    jobEntry.waitingForOrder = false
    orderEntry.actor ! OrderActor.Input.StartProcessing(jobKey, job, jobEntry.actor)
  }

  private def tryExecuteInstruction(order: Order[Order.State], workflow: Workflow): Unit = {
    assert(order.isAttachedToAgent)
    for (KeyedEvent(orderId, event) ← orderProcessor.nextEvent(order.id).onProblem(p ⇒ logger.error(p)).flatten) {
      orderRegister(orderId).actor ! OrderActor.Input.HandleEvent(event)
    }
  }

  private def removeOrder(orderId: OrderId): Unit = {
    for (orderEntry ← orderRegister.get(orderId)) {
      orderEntry.actor ! OrderActor.Input.Terminate
      //context.unwatch(orderEntry.actor)
      orderRegister.remove(orderId)
    }
  }

  override def unhandled(message: Any) =
    message match {
      case Terminated(actorRef) if jobRegister contains actorRef ⇒
        val jobKey = jobRegister.actorToKey(actorRef)
        if (terminating) {
          logger.debug(s"Actor '$jobKey' stopped")
        } else {
          logger.error(s"Actor '$jobKey' stopped unexpectedly")
        }
        jobRegister.onActorTerminated(actorRef)
        termination.continue()

      case Terminated(actorRef) if orderRegister contains actorRef ⇒
        val orderId = orderRegister(actorRef).order.id
        logger.debug(s"Actor '$orderId' stopped")
        orderRegister.onActorTerminated(actorRef)
        termination.continue()

      case Terminated(`journalActor`) if terminating ⇒
        context.stop(self)

      case Internal.StillTerminating ⇒
        termination.onStillTerminating()

      case _ ⇒
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
    final case class ExternalCommand(command: OrderCommand, response: Promise[Response])
  }

  private object Internal {
    final case class ContinueAttachOrder(cmd: AgentCommand.AttachOrder, promise: Promise[Accepted])
    final case class Due(orderId: OrderId)
    object StillTerminating extends DeadLetterSuppression
  }
}
