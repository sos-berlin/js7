package com.sos.jobscheduler.agent.scheduler.order

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Stash, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, AttachJobnet, AttachOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand, Response}
import com.sos.jobscheduler.agent.scheduler.event.EventQueue
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.Jobnet.EndNode
import com.sos.jobscheduler.data.jobnet.JobnetEvent.JobnetAttached
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderStepEnded}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.startJournalAndFinishRecovery
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor, KeyedJournalingActor}
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.Duration
import java.time.Instant.now
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
extends KeyedEventJournalingActor[JobnetEvent] with Stash {

  import context.{actorOf, dispatcher, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  protected val journalActor = actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val jobRegister = new JobRegister
  private val jobnetRegister = new JobnetRegister
  private val orderRegister = new OrderRegister(timerService)
  private val eventsForMaster = actorOf(Props { new EventQueue(timerService) }, "eventsForMaster")
  private var terminating = false

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  private def recover(): Unit = {
    val recoverer = new OrderJournalRecoverer(journalFile = journalFile, eventsForMaster = eventsForMaster)
    recoverer.recoverAll()
    for (jobnet ← recoverer.jobnets)
      jobnetRegister.recover(jobnet)
    for (order ← recoverer.orders) {
      val actor = newOrderActor(order.id)
      orderRegister.recover(order, actor)
      actor ! KeyedJournalingActor.Input.Recover(order)
    }
    startJournalAndFinishRecovery(journalActor = journalActor, orderRegister.recoveredJournalingActors)
  }

  override def postStop() = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def snapshots = {
    val jobnetSnapshots = jobnetRegister.jobnets
    for (eventQueueSnapshot ← (eventsForMaster ? EventQueue.Input.GetSnapshot).mapTo[EventQueue.CompleteSnapshot])
      yield jobnetSnapshots :+ eventQueueSnapshot  // Future: don't use mutable `this`
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
      handleAddedOrder(order.id)

    case JsonJournalRecoverer.Output.JournalIsReady ⇒
     logger.info(s"${jobnetRegister.size} Jobnets recovered, ${orderRegister.size} Orders recovered")
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
      eventsForMaster.forward(EventQueue.Input.RequestEvents(after, timeout, limit, promise))

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

    case JobActor.Output.ReadyForOrder if (jobRegister contains sender()) && !terminating ⇒
      tryStartStep(jobRegister(sender()))

    case Internal.Due(orderId) if orderRegister contains orderId ⇒
      val orderEntry = orderRegister(orderId)
      onOrderAvailable(orderEntry)

    case stamped @ Stamped(_, KeyedEvent(_: OrderId, _: OrderEvent)) ⇒
      eventsForMaster ! stamped
  }

  private def processOrderCommand(cmd: OrderCommand): Future[Response] = cmd match {
    case AttachJobnet(jobnet) if !terminating ⇒
      jobnetRegister.get(jobnet.path) match {
        case None ⇒
          persistFuture(KeyedEvent(JobnetAttached(jobnet.inputNodeId, jobnet.idToNode))(jobnet.path)) { stampedEvent ⇒
            jobnetRegister.handleEvent(stampedEvent.value)
            Accepted
          }
        case Some(`jobnet`) ⇒
          Future.successful(Accepted)
        case Some(_) ⇒
          Future.failed(new IllegalStateException(s"Changed ${jobnet.path}"))
      }

    case AttachOrder(order) if !terminating ⇒
      import order.nodeKey
      jobnetRegister.get(nodeKey.jobnetPath) match {
        case Some(jobnet) if jobnet isDefinedAt nodeKey.nodeId ⇒
          if (orderRegister contains order.id) {
            // May occur after Master restart when Master is not sure about order has been attached previously.
            logger.debug(s"Ignoring duplicate $cmd")
            Future.successful(Accepted)
          } else {
            attachOrder(order) map { case Completed ⇒ Accepted }
          }
        case Some(_) ⇒
          Future.failed(new IllegalArgumentException(s"Unknown NodeId ${nodeKey.nodeId} in ${nodeKey.jobnetPath}"))
        case None ⇒
          Future.failed(new IllegalArgumentException(s"Unknown ${nodeKey.jobnetPath}"))
      }

    case DetachOrder(orderId) ⇒
      orderRegister.get(orderId) match {
        case Some(orderEntry) ⇒
          orderEntry.detaching = true  // OrderActor is terminating
          (orderEntry.actor ? OrderActor.Command.Detach).mapTo[Completed] map { _ ⇒ Accepted }
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
    orderRegister.get(orderId) match {
      case Some(orderEntry) ⇒
        body(orderEntry)
      case None ⇒
        Future.failed(new IllegalArgumentException(s"Unknown $orderId"))
    }

  private def attachOrder(order: Order[Order.Idle]): Future[Completed] = {
    val actor = newOrderActor(order.id)
    orderRegister.insert(order, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]
    // Now expecting OrderEvent.OrderAttached
  }

  private def newOrderActor(orderId: OrderId) =
    watch(actorOf(
      Props { new OrderActor(orderId, journalActor = journalActor, config) },
      name = encodeAsActorName(s"Order-${orderId.string}")))

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent): Unit = {
    val orderEntry = orderRegister(order.id)
    event match {
      case _: OrderAttached ⇒
        orderEntry.order = order
        handleAddedOrder(order.id)

      case e: OrderStepEnded ⇒
        assert(order.nodeId == e.nextNodeId)
        assert(order.state == Order.Waiting)
        val fromNode = jobnetRegister.nodeKeyToJobNodeOption(orderEntry.order.nodeKey).get
        for (jobEntry ← jobRegister.get(fromNode.jobPath))  // Job may be stopped
          jobEntry.queue -= order.id
        orderEntry.order = order
        onOrderAvailable(orderEntry)

      case _ ⇒
        orderEntry.order = order
    }
  }

  private def handleAddedOrder(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    orderEntry.order.state match {
      case Order.Scheduled(instant) if now < instant ⇒
        orderEntry.at(instant) {  // TODO Register only the next order in TimerService ?
          self ! Internal.Due(orderId)
        }

      case Order.Scheduled(_) | Order.StartNow | Order.Waiting | Order.Detached/*???*/ ⇒
        onOrderAvailable(orderEntry)

      case _ ⇒
    }
  }

  private def onOrderAvailable(orderEntry: OrderEntry): Unit =
    jobnetRegister.nodeKeyToNodeOption(orderEntry.order.nodeKey) match {
      case Some(node: Jobnet.JobNode) if !terminating ⇒
        jobRegister.get(node.jobPath) match {
          case Some(jobEntry) ⇒
            onOrderAvailableForJob(orderEntry.order.id, jobEntry)
          case None ⇒
            logger.error(s"Missing '${node.jobPath}' for '${orderEntry.order.id}' at '${orderEntry.order.nodeKey}'")
        }

      case Some(node: Jobnet.JobNode) if terminating ⇒
        logger.info(s"Due to termination, processing of ${orderEntry.order.id} stops at ${node.id}")

      case Some(_: EndNode) | None ⇒
        if (!terminating) {  // When terminating, the order actors are terminating now
          logger.trace(s"${orderEntry.order.id} is ready to be retrieved by the Master")
          orderRegister(orderEntry.order.id).actor ! OrderActor.Input.SetReady
        }
    }

  private def onOrderAvailableForJob(orderId: OrderId, jobEntry: JobEntry): Unit = {
    logger.trace(s"$orderId is queuing for ${jobEntry.jobPath}")
    jobEntry.queue += orderId
    if (jobEntry.waitingForOrder) {
      tryStartStep(jobEntry)
    } else {
      jobEntry.actor ! JobActor.Input.OrderAvailable
    }
  }

  private def tryStartStep(jobEntry: JobEntry): Unit = {
    jobEntry.queue.dequeue() match {
      case Some(orderId) ⇒
        orderRegister.get(orderId) match {
          case None ⇒
            logger.warn(s"Unknown $orderId was enqueued for ${jobEntry.jobPath}. Order has been removed?")  // TODO Why can this happen?

          case Some(orderEntry) ⇒
            jobnetRegister.nodeKeyToNodeOption(orderEntry.order.nodeKey) match {
              case Some(node: Jobnet.JobNode) ⇒
                startStep(orderEntry, node, jobEntry)
              case _ ⇒
                logger.error(s"${orderEntry.order.id}: ${orderEntry.order.nodeKey} does not denote a JobNode")
            }
            jobEntry.waitingForOrder = false
        }

      case None ⇒
        jobEntry.waitingForOrder = true
    }
  }

  private def startStep(orderEntry: OrderEntry, node: Jobnet.JobNode, jobEntry: JobEntry): Unit = {
    logger.trace(s"${orderEntry.order.id} is going to be processed by ${jobEntry.jobPath}")
    assert(node.jobPath == jobEntry.jobPath)
    jobEntry.waitingForOrder = false
    orderEntry.actor ! OrderActor.Input.StartStep(node, jobEntry.actor)
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

  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[Jobnet],
    Subtype[Order[Order.State]],
    Subtype[EventQueue.CompleteSnapshot])

  private[order] val MyJournalMeta = new JsonJournalMeta[Event](SnapshotJsonFormat, AgentKeyedEventJsonFormat) with GzipCompression

  sealed trait Input
  object Input {
    final case class Start(jobs: Seq[(JobPath, ActorRef)]) extends Input
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[EventQueue.MyEventSeq]) extends Input
    final case class ExternalCommand(command: OrderCommand, response: Promise[Response])
    final case object Terminate
  }

  private object Internal {
    final case class Due(orderId: OrderId)
  }
}
