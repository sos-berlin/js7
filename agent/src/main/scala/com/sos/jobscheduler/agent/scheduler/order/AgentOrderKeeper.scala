package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Props, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.{AttachJobnet, AttachOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand}
import com.sos.jobscheduler.agent.scheduler.event.EventQueue
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.jobscheduler.agent.scheduler.job.JobRunner
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.agent.scheduler.order.JobRegister.JobEntry
import com.sos.jobscheduler.agent.scheduler.order.OrderRegister.OrderEntry
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.JobnetEvent.JobnetAttached
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetEvent, JobnetPath}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderNodeChanged, OrderStepEnded}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
import java.nio.file.Path
import java.time.Duration
import java.time.Instant.now
import scala.collection.immutable.Seq
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

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
  implicit private val timerService: TimerService)
extends KeyedEventJournalingActor[JobnetEvent] with Stash {

  import context.dispatcher

  protected val journalActor = context.actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val eventsForMaster = context.actorOf(
    Props { new EventQueue(timerService) },
    "eventsForMaster")
  private val jobRegister = new JobRegister
  private val jobnetRegister = new JobnetRegister
  private val orderRegister = new OrderRegister(timerService)

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
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

  private def recover(): Unit = {
    val recoverer = new JsonJournalRecoverer(MyJournalMeta, journalFile) {
      def recoverSnapshot = {
        case jobnet: Jobnet ⇒
          jobnetRegister.recover(jobnet)

        case order: Order[Order.State] ⇒
          val actor = addOrderActor(order.id)
          orderRegister.insert(order, actor)
          recoverActorForSnapshot(order, actor)

        case snapshot: EventQueue.CompleteSnapshot ⇒
          eventsForMaster ! snapshot  // TODO FinishRecovery for synchronization ?
      }

      def recoverNewKey = {
        case Stamped(_, KeyedEvent(path: JobnetPath, event: JobnetEvent.JobnetAttached)) ⇒
          jobnetRegister.handleEvent(KeyedEvent(event)(path))

        case stamped @ Stamped(_, KeyedEvent(orderId: OrderId, event: OrderEvent.OrderAttached)) ⇒
          val actor = addOrderActor(orderId)
          orderRegister.handleOrderAttached(KeyedEvent(event)(orderId), actor)
          recoverActorForNewKey(stamped, actor)
          eventsForMaster ! stamped
      }

      override def onDeletedRecovered = {
        case stamped @ Stamped(_, KeyedEvent(orderId: OrderId, OrderEvent.OrderDetached)) ⇒
          // OrderActor stops itself
          context.unwatch(orderRegister(orderId).actor)
          orderRegister.handleOrderDetached(KeyedEvent(OrderEvent.OrderDetached)(orderId))
          eventsForMaster ! stamped
      }

      override def onChangedRecovered = {
        case stamped @ Stamped(_, KeyedEvent(_: OrderId, _: OrderEvent)) ⇒
          eventsForMaster ! stamped
      }
    }
    recoverer.recoverAllAndSendTo(journalActor = journalActor)
  }

  def receive = journaling orElse {
    case JsonJournalActor.Output.Ready ⇒
      for (o ← orderRegister.values) o.actor ! OrderActor.Input.FinishRecovery  // Responds with OrderActor.Output.RecoveryFinished
      context.become(startable)
      logger.info(s"${jobnetRegister.size} Jobnets recovered, ${orderRegister.size} Orders recovered")
      unstashAll()

    case _ ⇒
      stash()
  }

  private def startable: Receive = journaling orElse {
    case Input.Start(jobPathsAndActors) ⇒
      for ((jobPath, actorRef) ← jobPathsAndActors) {
        jobRegister.insert(jobPath, actorRef)
        context.watch(actorRef)
      }
      context.become(ready)
      unstashAll()
      logger.info("Ready")

    case _ ⇒
      stash()
  }

  private def ready: Receive = journaling orElse {
    case OrderActor.Output.RecoveryFinished(order) ⇒  // TODO Ist es gut, schon bereit zu sein, während noch die Aufträge ihre Wiederherstellung bestätigen? Was wenn währenddessen ein Kommando kommt?
      //logger.info(s"Recovered: $order")
      handleAddedOrder(order)

    case cmd: OrderCommand ⇒
      processOrderCommand(cmd)

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      handleOrderEvent(order, event)

    case Internal.Due(orderId) if orderRegister contains orderId ⇒
      val orderEntry = orderRegister(orderId)
      onOrderAvailable(orderEntry)

    case stamped @ Stamped(_, KeyedEvent(orderId: OrderId, _: OrderEvent)) if orderRegister contains orderId ⇒
      eventsForMaster ! stamped

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      eventsForMaster.forward(EventQueue.Input.RequestEvents(after, timeout, limit, promise))

    case JobRunner.Output.ReadyForOrder if jobRegister contains sender() ⇒
      tryStartStep(jobRegister(sender()))
  }

  private def processOrderCommand(cmd: OrderCommand) = cmd match {
    case AttachJobnet(jobnet) ⇒
      jobnetRegister.get(jobnet.path) match {
        case None ⇒
          persist(KeyedEvent(JobnetAttached(jobnet.inputNodeId, jobnet.idToNode))(jobnet.path)) { stampedEvent ⇒
            jobnetRegister.handleEvent(stampedEvent.value)
            sender() ! EmptyResponse
          }
        case Some(`jobnet`) ⇒
          sender() ! EmptyResponse
        case Some(_) ⇒
          sender() ! Status.Failure(new IllegalStateException(s"Changed ${jobnet.path}"))
      }

    case AttachOrder(order) ⇒
      import order.nodeKey
      jobnetRegister.get(nodeKey.jobnetPath) match {
        case Some(jobnet) if jobnet isDefinedAt nodeKey.nodeId ⇒
          attachOrder(order) map { case Completed ⇒ EmptyResponse } pipeTo sender()
        case Some(_) ⇒
          sender() ! Status.Failure(new IllegalArgumentException(s"Unknown NodeId ${nodeKey.nodeId} in ${nodeKey.jobnetPath}"))
        case None ⇒
          sender() ! Status.Failure(new IllegalArgumentException(s"Unknown ${nodeKey.jobnetPath}"))
      }

    case DetachOrder(orderId) ⇒
      executeCommandForOrderId(orderId) { orderEntry ⇒
        context.unwatch(orderEntry.actor)
        (orderEntry.actor ? OrderActor.Command.Detach).mapTo[Completed] map { _ ⇒ EmptyResponse } pipeTo sender()
      }

    case GetOrder(orderId) ⇒
      executeCommandForOrderId(orderId) { orderEntry ⇒
        (orderEntry.actor ? OrderActor.Command.GetSnapshot) map {
          case order: Order[Order.State] ⇒ GetOrder.Response(order)
        } pipeTo sender()
      }

    case GetOrderIds ⇒
      sender() ! GetOrderIds.Response(orderRegister.keys)

    case GetOrders ⇒
      // The order snapshots are fetched asynchronously.
      // Under a running Agent, the result may be an inconsistent snapshot (dirty read).
      val whenResponded: Future[GetOrders.Response] =
        Future.sequence(
          for (orderEntry ← orderRegister.values) yield
            (orderEntry.actor ? OrderActor.Command.GetSnapshot).mapTo[Order[Order.State]] map Some.apply recover {
              case NonFatal(t) ⇒  // Should we ignore an inconsistent snapshot with some died actors ???
                logger.warn(s"GetSnapshot ${orderEntry.orderId} throwed $t")
                None
            }
        ) map { _.flatten } map GetOrders.Response
      whenResponded pipeTo sender()
  }

  private def executeCommandForOrderId(orderId: OrderId)(body: OrderEntry ⇒ Unit) =
    orderRegister.get(orderId) match {
      case Some(orderEntry) ⇒
        body(orderEntry)
      case None ⇒
        sender() ! Status.Failure(new IllegalArgumentException(s"Unknown $orderId"))
    }

  private def attachOrder(order: Order[Order.Idle]): Future[Completed] = {
    val actor = addOrderActor(order.id)
    orderRegister.insert(order, actor)
    (actor ? OrderActor.Command.Attach(order)).mapTo[Completed]
    // Now expecting OrderEvent.OrderAttached
  }

  private def addOrderActor(orderId: OrderId) = {
    val actor = context.actorOf(
      Props { new OrderActor(orderId, journalActor = journalActor) },
      name = encodeAsActorName(s"Order-${orderId.string}"))
    context.watch(actor)
    actor
  }

  private def handleOrderEvent(order: Order[Order.State], event: OrderEvent) = {
    val orderEntry = orderRegister(order.id)
    import orderEntry.orderId
    event match {
      case _: OrderAttached ⇒
        handleAddedOrder(order)

      case _: OrderStepEnded ⇒
        for (jobNode ← jobnetRegister.nodeKeyToJobNodeOption(orderEntry.nodeKey);
             jobEntry ← jobRegister.get(jobNode.jobPath)) {
          jobEntry.queue -= orderId
        }

      case e: OrderNodeChanged ⇒
        orderEntry.nodeId = e.nodeId
        onOrderAvailable(orderEntry)

      case _ ⇒
    }
  }

  private def handleAddedOrder(order: Order[Order.State]): Unit = {
    val orderEntry = orderRegister(order.id)
    import orderEntry.orderId
    order.state match {
      case Order.Scheduled(instant) if now < instant ⇒
        orderEntry.at(instant) {
          self ! Internal.Due(orderId)
        }

      case Order.Scheduled(_) | Order.StartNow ⇒
        onOrderAvailable(orderEntry)

      case Order.Waiting | Order.Detached/*???*/ ⇒
        orderEntry.nodeId = order.nodeId
        onOrderAvailable(orderEntry)

      case _ ⇒
    }
  }

  private def onOrderAvailable(orderEntry: OrderEntry): Unit =
    jobnetRegister.nodeKeyToNodeOption(orderEntry.nodeKey) match {
      case Some(node: Jobnet.JobNode) ⇒
        jobRegister.get(node.jobPath) match {
          case Some(jobEntry) ⇒
            logger.trace(s"${orderEntry.orderId} is queuing for ${jobEntry.jobPath}")
            jobEntry.queue += orderEntry.orderId
            if (jobEntry.waitingForOrder) {
              tryStartStep(jobEntry)
            } else {
              jobEntry.actor ! JobRunner.Input.OrderAvailable
            }
          case None ⇒
            logger.warn(s"Missing ${node.jobPath} for ${orderEntry.orderId}")
        }
      case _ ⇒
        logger.trace(s"${orderEntry.orderId} is ready to be retrieved by the Master")
        orderRegister(orderEntry.orderId).actor ! OrderActor.Input.SetReady
    }

  private def tryStartStep(jobEntry: JobEntry): Unit = {
    jobEntry.queue.dequeue() match {
      case Some(orderId) ⇒
        val orderEntry = orderRegister(orderId)
        jobnetRegister.nodeKeyToNodeOption(orderEntry.nodeKey) match {
          case Some(jobNode: Jobnet.JobNode) ⇒
            logger.trace(s"${orderEntry.orderId} is going to be processed by ${jobEntry.jobPath}")
            assert(jobNode.jobPath == jobEntry.jobPath)
            jobEntry.waitingForOrder = false
            orderEntry.actor ! OrderActor.Input.StartStep(jobNode, jobEntry.actor)
          case _ ⇒
            logger.warn(s"${orderEntry.orderId}: ${orderEntry.nodeKey} does not denote a JobNode")
        }
      case None ⇒
        jobEntry.waitingForOrder = true
    }
  }

  override def unhandled(message: Any) =
    message match {
      case Terminated(actorRef) if jobRegister contains actorRef ⇒
        val jobPath = jobRegister.actorToKey(actorRef)
        val msg = s"Job Actor '${jobPath.string}' terminated unexpectedly"
        logger.warn(msg)
        jobRegister.onActorTerminated(actorRef)

      case Terminated(actorRef) if orderRegister contains actorRef ⇒
        orderRegister.onActorTerminated(actorRef)

      case _ ⇒
        super.unhandled(message)
    }
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[Jobnet],
    Subtype[Order[Order.State]],
    Subtype[EventQueue.CompleteSnapshot])

  private val MyJournalMeta = new JsonJournalMeta[Event](
      SnapshotJsonFormat,
      AgentKeyedEventJsonFormat,
      snapshotToKey = {
        case jobnet: Jobnet ⇒ jobnet.path
        case order: Order[_] ⇒ order.id
      },
      isDeletedEvent = Set(OrderEvent.OrderDetached))
    with GzipCompression

  sealed trait Input
  object Input {
    final case class Start(jobs: Seq[(JobPath, ActorRef)]) extends Input
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[EventQueue.MyEventSeq]) extends Input
  }

  private object Internal {
    final case class Due(orderId: OrderId)
  }
}
