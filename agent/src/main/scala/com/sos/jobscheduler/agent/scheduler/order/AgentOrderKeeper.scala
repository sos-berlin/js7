package com.sos.jobscheduler.agent.scheduler.order

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import cats.data.Validated.{Invalid, Valid}
import com.softwaremill.tagging.Tagger
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, AttachOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand, Response}
import com.sos.jobscheduler.agent.scheduler.event.EventQueueActor
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked.ops._
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.Exceptions.wrapException
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalRecoverer.startJournalAndFinishRecovery
import com.sos.jobscheduler.core.event.journal.{JournalActor, JournalMeta, JournalRecoverer, KeyedEventJournalingActor, KeyedJournalingActor}
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowEvent.WorkflowAttached
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow, WorkflowEvent}
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.Duration
import scala.collection.immutable.Seq
import scala.concurrent.{Future, Promise}

/**
  * Keeper of one Master's orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  journalFile: Path,
  implicit private val askTimeout: Timeout,
  syncOnCommit: Boolean,
  keyedEventBus: StampedKeyedEventBus,
  eventIdGenerator: EventIdGenerator,
  config: Config,
  implicit private val timerService: TimerService)
extends KeyedEventJournalingActor[WorkflowEvent] with Stash {

  import context.{actorOf, dispatcher, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor = actorOf(
    JournalActor.props(
      journalMeta(compressWithGzip = config.getBoolean("jobscheduler.agent.journal.gzip")),
      journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus),
    "Journal")
  private val jobRegister = new JobRegister
  private val workflowRegister = new WorkflowRegister
  private val orderRegister = new OrderRegister(timerService)
  private val orderProcessor = new OrderProcessor(workflowRegister.pathToWorkflow, orderRegister.idToOrder)
  private val eventsForMaster = actorOf(Props { new EventQueueActor(timerService) }, "eventsForMaster").taggedWith[EventQueueActor]

  private var terminating = false

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  private def recover(): Unit = {
    val recoverer = new OrderJournalRecoverer(journalFile = journalFile, eventsForMaster)(askTimeout)
    recoverer.recoverAll()
    for (namedWorkflow ← recoverer.namedWorkflows)
      wrapException(s"Error when recovering ${namedWorkflow.path}") {
        workflowRegister.recover(namedWorkflow)
      }
    for (recoveredOrder ← recoverer.orders)
      wrapException(s"Error when recovering ${recoveredOrder.id}") {
        val order = workflowRegister.reuseMemory(recoveredOrder)
        val workflow = workflowRegister(order.workflowPath).workflow  // Workflow must be recovered
        val actor = newOrderActor(order)
        orderRegister.recover(order, workflow, actor)
        actor ! KeyedJournalingActor.Input.Recover(order)
      }
    startJournalAndFinishRecovery(journalActor = journalActor, orderRegister.recoveredJournalingActors)
  }

  override def postStop() = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def snapshots = {
    val workflowSnapshots = workflowRegister.namedWorkflows
    for (got ← (eventsForMaster ? EventQueueActor.Input.GetSnapshots).mapTo[EventQueueActor.Output.GotSnapshots])
      yield workflowSnapshots ++ got.snapshots  // Future: don't use mutable `this`
  }

  def receive = journaling orElse {
    case Input.Start(jobPathsAndActors) ⇒
      for ((jobPath, actorRef) ← jobPathsAndActors) {
        jobRegister.insert(jobPath, actorRef)
        watch(actorRef)
      }
      context.become(awaitJournalIsReady)
      unstashAll()

    case _ ⇒
      stash()  // We stash all early OrderActor.Output.RecoveryFinished until the jobs are defined (Input.Start)
  }

  private def awaitJournalIsReady: Receive = journaling orElse {
    case OrderActor.Output.RecoveryFinished(order) ⇒
      orderRegister(order.id).order = order
      proceedWithOrder(order.id)

    case JournalRecoverer.Output.JournalIsReady ⇒
      logger.info(s"${workflowRegister.size} Workflows and ${orderRegister.size} Orders recovered")
      context.become(ready)
      unstashAll()
      logger.info("Ready")

    case _ ⇒
      stash()
  }

  private def ready: Receive = journaling orElse {
    case Input.ExternalCommand(cmd, response) ⇒
      response.completeWith(processOrderCommand(cmd))

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      eventsForMaster.forward(EventQueueActor.Input.RequestEvents(after, timeout, limit, promise))

    case Input.Terminate ⇒
      if (!terminating) {
        terminating = true
        for (o ← orderRegister.values if !o.detaching) {
          o.actor ! OrderActor.Input.Terminate
        }
      }
      eventsForMaster ! PoisonPill
      checkActorStop()
      sender() ! Done

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      handleOrderEvent(order, event)
      proceedWithOrder(order.id)

    case JobActor.Output.ReadyForOrder if (jobRegister contains sender()) && !terminating ⇒
      tryStartProcessing(jobRegister(sender()))

    case Internal.ContinueAttachOrder(cmd @ AttachOrder(order, workflow), promise) ⇒
      promise completeWith {
        if (!workflow.isDefinedAt(order.position))
          Future.failed(new IllegalArgumentException(s"Unknown Position ${order.position} in ${order.workflowPath}"))
        else
          if (orderRegister contains order.id) {
            // May occur after Master restart when Master is not sure about order has been attached previously.
            logger.debug(s"Ignoring duplicate $cmd")
            Future.successful(Accepted)
          } else
            attachOrder(workflowRegister.reuseMemory(order), workflow) map { case Completed ⇒ Accepted }
      }

    case Internal.Due(orderId) if orderRegister contains orderId ⇒
      val orderEntry = orderRegister(orderId)
      onOrderAvailable(orderEntry)

    case stamped @ Stamped(_, _, KeyedEvent(_: OrderId, event: OrderEvent)) if !terminating ⇒
      event match {
        case OrderDetached ⇒
          (eventsForMaster ? stamped) await 2 * askTimeout.duration.toJavaDuration  // blocking !!!
        case _ ⇒
          eventsForMaster ! stamped
      }
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Response] = cmd match {
    case cmd @ AttachOrder(order, workflow) if !terminating ⇒
      order.attachedToAgent match {
        case Invalid(problem) ⇒ Future.failed(problem.throwable)
        case Valid(_) ⇒
          val workflowResponse = workflowRegister.get(order.workflowPath) map (_.workflow) match {
            case None ⇒
              persistFuture(KeyedEvent(WorkflowAttached(workflow))(order.workflowPath)) { stampedEvent ⇒
                workflowRegister.handleEvent(stampedEvent.value)
                Accepted
              }
            case Some(w) if w.withoutSource == workflow.withoutSource ⇒
              Future.successful(Accepted)
            case Some(_) ⇒
              Future.failed(new IllegalStateException(s"Changed ${order.workflowPath}"))
          }
          workflowResponse flatMap { case Accepted ⇒
            promiseFuture[Accepted.type] { promise ⇒
              self ! Internal.ContinueAttachOrder(cmd, promise)
            }
          }
      }

    case DetachOrder(orderId) ⇒
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

    case _ if terminating ⇒
      Future.failed(new IllegalStateException(s"Agent is terminating"))
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
      name = uniqueOrderActorName(order.id)))

  private def uniqueOrderActorName(orderId: OrderId): String = {
    var name = encodeAsActorName(s"Order-${orderId.string}")
    if (context.child(name).isDefined) {  // May occur then a (child) order is attached again to this Agent and the Actor has not yet terminated
      name = Iterator.from(2).map(i ⇒ s"$name~$i").find { nam ⇒ context.child(nam).isEmpty }.get
      logger.debug(s"Duplicate Order actor name. Replacement actor name is $name")
    }
    name
  }

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent): Unit = {
    val orderEntry = orderRegister(order.id)
    val validatedFollowUps = orderProcessor.handleEvent(order.id <-: event)
    for (followUps ← validatedFollowUps onProblem (p ⇒ logger.error(p))) {
      followUps foreach {
        case FollowUp.Processed(job) ⇒
          assert(orderEntry.jobOption map (_.jobPath) contains job.jobPath)
          for (jobEntry ← jobRegister.checked(job.jobPath) onProblem (p ⇒ logger.error(p withKey order.id))) {
            jobEntry.queue -= order.id
          }

        case FollowUp.AddChild(childOrder) ⇒
          val actor = newOrderActor(childOrder)
          orderRegister.insert(childOrder, workflowRegister(childOrder.workflowPath).workflow, actor)
          actor ! OrderActor.Input.AddChild(childOrder)
          proceedWithOrder(childOrder.id)

        case FollowUp.Remove(removeOrderId) ⇒
          removeOrder(removeOrderId)

        case o: FollowUp.AddOffered ⇒
          sys.error(s"Unexpeced FollowUp $o")  // Only Master handles this
      }
    }
    orderEntry.order = order
  }

  private def proceedWithOrder(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    val order = orderEntry.order
    if (order.isAttachedToAgent) {
      order.state match {
        case Order.Scheduled(instant) if now < instant ⇒
          orderEntry.at(instant) {  // TODO Register only the next order in TimerService ?
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
    }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit = {
    logger.trace(s"$orderId is queuing for ${jobEntry.jobPath}")
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
    logger.trace(s"${orderEntry.order.id} is going to be processed by ${jobEntry.jobPath}")
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
          logger.debug(s"Actor $jobPath has stopped")
        } else {
          logger.error(s"Actor '$jobPath' has stopped unexpectedly")
        }
        jobRegister.onActorTerminated(actorRef)
        checkActorStop()

      case Terminated(actorRef) if orderRegister contains actorRef ⇒
        val orderId = orderRegister(actorRef).order.id
        logger.debug(s"Actor '$orderId' stopped")
        orderRegister.onActorTerminated(actorRef)
        checkActorStop()

      case _ ⇒
        super.unhandled(message)
    }

  private def checkActorStop() = {
    if (terminating && orderRegister.isEmpty && jobRegister.isEmpty) {
      context.stop(self)
    }
  }

  override def toString = "AgentOrderKeeper"
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[Workflow.Named],
    Subtype[Order[Order.State]],
    Subtype[EventQueueActor.Snapshot])

  private[order] def journalMeta(compressWithGzip: Boolean) =
    JournalMeta.gzipped[Event](SnapshotJsonFormat, AgentKeyedEventJsonCodec, compressWithGzip = compressWithGzip)

  sealed trait Input
  object Input {
    final case class Start(jobs: Seq[(JobPath, ActorRef)]) extends Input
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[EventQueueActor.MyEventSeq]) extends Input
    final case class ExternalCommand(command: OrderCommand, response: Promise[Response])
    final case object Terminate
  }

  private object Internal {
    final case class ContinueAttachOrder(cmd: AgentCommand.AttachOrder, promise: Promise[Accepted.type])
    final case class Due(orderId: OrderId)
  }
}
