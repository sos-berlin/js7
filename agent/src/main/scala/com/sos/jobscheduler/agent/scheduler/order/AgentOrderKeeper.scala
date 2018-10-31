package com.sos.jobscheduler.agent.scheduler.order

import akka.Done
import akka.actor.{ActorRef, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, AttachOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, KeepEvents, OrderCommand, Response}
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
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
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.WorkflowEvent.WorkflowAttached
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.ZoneId
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.{Future, Promise}

/**
  * Keeper of one Master's orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  masterId: MasterId,
  journalFileBase: Path,
  implicit private val askTimeout: Timeout,
  keyedEventBus: StampedKeyedEventBus,
  config: Config,
  implicit private val timerService: TimerService)(
  implicit scheduler: Scheduler)
extends MainJournalingActor[Event] with Stash {

  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val journalMeta = JournalMeta(SnapshotJsonFormat, AgentKeyedEventJsonCodec, journalFileBase)
  private val eventWatch = new JournalEventWatch[Event](journalMeta, config)
  protected val journalActor = watch(actorOf(
    JournalActor.props(journalMeta, config, keyedEventBus, scheduler),
    "Journal"))
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val orderRegister = new OrderRegister(timerService)
  private val orderProcessor = new OrderProcessor(workflowRegister.idToWorkflow.checked, orderRegister.idToOrder)
  private var terminating = false

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
    eventWatch.close()
    super.postStop()
  }

  def snapshots = Future.successful(workflowRegister.workflows)

  def receive = {
    case Input.Start(jobPathsAndActors) ⇒
      for ((jobPath, actorRef) ← jobPathsAndActors) {
        jobRegister.insert(jobPath, actorRef)
        watch(actorRef)
      }
      become("awaitJournalIsReady")(awaitJournalIsReady)
      unstashAll()

    case _ ⇒
      stash()  // We stash all early OrderActor.Output.RecoveryFinished until the jobs are defined (Input.Start)
  }

  private def awaitJournalIsReady: Receive = {
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

    case _ ⇒
      stash()
  }

  private def ready: Receive = {
    case Input.ExternalCommand(cmd, response) ⇒
      response.completeWith(processOrderCommand(cmd))

    case Input.GetEventWatch ⇒
      sender() ! eventWatch

    case Input.Terminate ⇒
      if (!terminating) {
        terminating = true
        journalActor ! JournalActor.Input.TakeSnapshot
      }
      sender() ! Done

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      if (!terminating) {
        handleOrderEvent(order, event)
        proceedWithOrder(order.id)
      }

    case JobActor.Output.ReadyForOrder if jobRegister contains sender() ⇒
      if (!terminating) {
        tryStartProcessing(jobRegister(sender()))
      }

    case JournalActor.Output.SnapshotTaken ⇒
      if (terminating) {
        for (o ← orderRegister.values if !o.detaching) {
          o.actor ! OrderActor.Input.Terminate
        }
        handleTermination()
      }

    case Internal.ContinueAttachOrder(cmd @ AttachOrder(order, workflow), promise) ⇒
      promise completeWith {
        if (!workflow.isDefinedAt(order.position))
          Future.failed(Problem.fromEager(s"Unknown Position ${order.workflowPosition}").throwable)
        else if (orderRegister contains order.id) {
          // May occur after Master restart when Master is not sure about order has been attached previously.
          logger.debug(s"Ignoring duplicate $cmd")
          Future.successful(Accepted)
        } else if (terminating)
          Future.failed(AgentIsTerminatingProblem.throwable)
        else
          attachOrder(workflowRegister.reuseMemory(order), workflow) map { case Completed ⇒ Accepted }
      }

    case Internal.Due(orderId) if orderRegister contains orderId ⇒
      if (!terminating) {
        val orderEntry = orderRegister(orderId)
        onOrderAvailable(orderEntry)
      }
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Response] = cmd match {
    case cmd @ AttachOrder(order, workflow) if !terminating ⇒
      order.attachedToAgent match {
        case Invalid(problem) ⇒ Future.failed(problem.throwable)
        case Valid(_) ⇒
          val workflowResponse = workflowRegister.get(order.workflowId) match {
            case None ⇒
              persist(WorkflowAttached(workflow)) { stampedEvent ⇒
                workflowRegister.handleEvent(stampedEvent.value)
                Accepted
              }
            case Some(w) if w.withoutSource == workflow.withoutSource ⇒
              Future.successful(Accepted)
            case Some(_) ⇒
              Future.failed(new IllegalStateException(s"Changed ${order.workflowId}"))
          }
          workflowResponse flatMap { case Accepted ⇒
            promiseFuture[Accepted.type] { promise ⇒
              self ! Internal.ContinueAttachOrder(cmd, promise)
            }
          }
      }

    case DetachOrder(orderId) if !terminating ⇒
      orderRegister.get(orderId) match {
        case Some(orderEntry) ⇒
          orderEntry.order.detachableFromAgent match {
            case Invalid(problem) ⇒ Future.failed(problem.throwable)
            case Valid(_) ⇒
              orderEntry.detaching = true  // OrderActor is terminating
              (orderEntry.actor ? OrderActor.Command.Detach).mapTo[Completed] map { _ ⇒ Accepted }  // TODO ask will time-out when Journal blocks
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
      Future.failed(AgentIsTerminatingProblem.throwable)
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
        case FollowUp.Processed(job) ⇒
          assert(orderEntry.jobOption exists (_.jobPath == job.jobPath))
          for (jobEntry ← jobRegister.checked(job.jobPath) onProblem (p ⇒ logger.error(p withKey order.id))) {
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
    for (_ ← orderEntry.order.attachedToAgent onProblem (p ⇒ logger.error(s"onOrderAvailable: $p"))) {
      orderEntry.instruction match {
        case job: Job ⇒
          jobRegister.get(job.jobPath) match {
            case None ⇒
              logger.error(s"Missing '${job.jobPath}' for '${orderEntry.order.id}' at '${orderEntry.order.workflowPosition}'")
            case Some(jobEntry) ⇒
              onOrderAvailableForJob(orderEntry.order.id, jobEntry)
          }

        case _ ⇒
          tryExecuteInstruction(orderEntry.order, orderEntry.workflow)
      }
    }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit = {
    logger.debug(s"$orderId is queuing for ${jobEntry.jobPath}")
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
            logger.warn(s"Unknown $orderId was enqueued for ${jobEntry.jobPath}. Order has been removed?")  // TODO Why can this happen?

          case Some(orderEntry) ⇒
            for (job ← orderEntry.checkedJob onProblem (o ⇒ logger.error(o))) {
              startProcessing(orderEntry, job, jobEntry)
            }
            jobEntry.waitingForOrder = false
        }

      case None ⇒
        jobEntry.waitingForOrder = true
    }

  private def startProcessing(orderEntry: OrderEntry, job: Job, jobEntry: JobEntry): Unit = {
    logger.debug(s"${orderEntry.order.id} is going to be processed by ${jobEntry.jobPath}")
    assert(job.jobPath == jobEntry.jobPath)
    jobEntry.waitingForOrder = false
    orderEntry.actor ! OrderActor.Input.StartProcessing(job, jobEntry.actor)
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
        val jobPath = jobRegister.actorToKey(actorRef)
        if (terminating) {
          logger.debug(s"Actor $jobPath stopped")
        } else {
          logger.error(s"Actor '$jobPath' stopped unexpectedly")
        }
        jobRegister.onActorTerminated(actorRef)
        handleTermination()

      case Terminated(actorRef) if orderRegister contains actorRef ⇒
        val orderId = orderRegister(actorRef).order.id
        logger.debug(s"Actor '$orderId' stopped")
        orderRegister.onActorTerminated(actorRef)
        handleTermination()

      case Terminated(`journalActor`) if terminating ⇒
        context.stop(self)

      case _ ⇒
        super.unhandled(message)
    }

  private def handleTermination() = {
    if (terminating && orderRegister.isEmpty && jobRegister.isEmpty) {
      journalActor ! JournalActor.Input.Terminate
    }
  }

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val AgentIsTerminatingProblem = Problem.fromEager("Agent is terminating")  // TODO 503 ServiceUnavailable? Master must not retry before new Login
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[Workflow],
    Subtype[Order[Order.State]])

  sealed trait Input
  object Input {
    final case class Start(jobs: Seq[(JobPath, ActorRef)]) extends Input
    final case object GetEventWatch
    final case class ExternalCommand(command: OrderCommand, response: Promise[Response])
    final case object Terminate
  }

  private object Internal {
    final case class ContinueAttachOrder(cmd: AgentCommand.AttachOrder, promise: Promise[Accepted.type])
    final case class Due(orderId: OrderId)
  }
}
