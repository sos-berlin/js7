package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Props, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.{AddJobnet, AddOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand}
import com.sos.jobscheduler.agent.scheduler.event.EventQueue
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.jobscheduler.agent.scheduler.job.JobRunner
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper._
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.JobnetEvent.JobnetAttached
import com.sos.jobscheduler.data.jobnet.{JobPath, Jobnet, JobnetEvent, JobnetPath, NodeId, NodeKey}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAttached, OrderNodeChanged, OrderStepEnded}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.shared.common.ActorRegister
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.{RecoveringChanged, RecoveringDeleted, RecoveringForUnknownKey, RecoveringSnapshot}
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, Journal, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
import java.nio.file.Path
import java.time.Duration
import java.time.Instant.now
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/**
  * Keeper of the orders.
  *
  * @author Joacim Zschimmer
  */
final class AgentOrderKeeper(
  journalFile: Path,
  implicit private val askTimeout: Timeout,
  syncOnCommit: Boolean,
  keyedEventBus: StampedKeyedEventBus,
  eventIdGenerator: EventIdGenerator,
  timerService: TimerService)
extends KeyedEventJournalingActor[JobnetEvent] with Stash {

  import context.dispatcher

  protected val journalActor = context.actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val jobRegister = new ActorRegister[JobPath, JobEntry](_.actor)
  private val pathToJobnet = mutable.Map[JobnetPath, Jobnet]()
  private val orderRegister = new ActorRegister[OrderId, OrderEntry](_.actor)
  private val eventsForMaster = context.actorOf(Props { new EventQueue(timerService) }, "eventsForMaster")

  def snapshots = {
    val jobnetSnapshots = pathToJobnet.values.toVector
    for (eventQueueSnapshot ← (eventsForMaster ? EventQueue.Input.GetSnapshot).mapTo[EventQueue.CompleteSnapshot])
      yield jobnetSnapshots :+ eventQueueSnapshot  // Future: don't use `this`
  }

  override def preStart(): Unit = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  private def recover(): Unit = {
    val recovered =
      autoClosing(new JsonJournalRecoverer(MyJournalMeta, journalFile)) { journal ⇒
        for (recovered ← journal) (recovered: @unchecked) match {
          case RecoveringSnapshot(jobnet: Jobnet) ⇒
            pathToJobnet += jobnet.path → jobnet

          case RecoveringSnapshot(order: Order[Order.State]) ⇒
            val actor = addOrderActor(order.id)
            orderRegister += order.id → OrderEntry(order.id, actor, order.jobnetPath, order.nodeId)
            journal.recoverActorForSnapshot(order, actor)

          case RecoveringSnapshot(snapshot: EventQueue.CompleteSnapshot) ⇒
            eventsForMaster ! snapshot  // TODO FinishRecovery for synchronization ?

          case RecoveringForUnknownKey(Stamped(_, KeyedEvent(path: JobnetPath, event: JobnetEvent.JobnetAttached))) ⇒
            pathToJobnet += path → Jobnet.fromJobnetAttached(path, event)

          case RecoveringForUnknownKey(stamped @ Stamped(_, KeyedEvent(orderId: OrderId, event: OrderEvent.OrderAttached))) ⇒
            val actor = addOrderActor(orderId)
            orderRegister += orderId → OrderEntry(orderId, actor, event.nodeKey.jobnetPath, event.nodeKey.nodeId)
            journal.recoverActorForFirstEvent(stamped, actor)
            eventsForMaster ! stamped

          case RecoveringDeleted(stamped @ Stamped(_, KeyedEvent(orderId: OrderId, OrderEvent.OrderDetached))) ⇒
            // OrderActor stops itself
            context.unwatch(orderRegister(orderId).actor)
            orderRegister -= orderId
            eventsForMaster ! stamped

          case RecoveringChanged(stamped @ Stamped(_, KeyedEvent(_: OrderId, _: OrderEvent))) ⇒
            eventsForMaster ! stamped
        }
        journal.recoveredJournalingActors
      }
    journalActor ! Journal.Input.Start(recovered)
  }

  override def postStop() = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def receive = journaling orElse {
    case Journal.Output.Ready ⇒
      for (o ← orderRegister.values) o.actor ! OrderActor.Input.FinishRecovery  // Responds with OrderActor.Output.RecoveryFinished
      context.become(startable)
      logger.info(s"${pathToJobnet.size} Jobnets recovered, ${orderRegister.size} Orders recovered")
      unstashAll()

    case _ ⇒
      stash()
  }

  private def startable: Receive = journaling orElse {
    case Input.Start(jobPathsAndActors) ⇒
      for ((jobPath, actorRef) ← jobPathsAndActors) {
        jobRegister += jobPath → new JobEntry(jobPath, actorRef)
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
      handleChangedOrderState(order)

    case cmd: OrderCommand ⇒
      processOrderCommand(cmd)

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      handleOrderEvent(order, event)

    case Internal.Due(orderId) if orderRegister isDefinedAt orderId ⇒
      val orderEntry = orderRegister(orderId)
      orderEntry.timer = None
      onOrderAvailable(orderEntry)

    case stamped @ Stamped(_, KeyedEvent(orderId: OrderId, _: OrderEvent)) if orderRegister contains orderId ⇒
      eventsForMaster ! stamped

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      eventsForMaster.forward(EventQueue.Input.RequestEvents(after, timeout, limit, promise))

    case JobRunner.Output.ReadyForOrder if jobRegister isDefinedAt sender() ⇒
      tryStartStep(jobRegister(sender()))
  }

  private def processOrderCommand(cmd: OrderCommand) = cmd match {
    case AddJobnet(jobnet) ⇒
      pathToJobnet.get(jobnet.path) match {
        case None ⇒
          persist(KeyedEvent(JobnetAttached(jobnet.inputNodeId, jobnet.idToNode))(jobnet.path)) { _ ⇒
            pathToJobnet += jobnet.path → jobnet
            sender() ! EmptyResponse
          }
        case Some(`jobnet`) ⇒
          sender() ! EmptyResponse
        case Some(_) ⇒
          sender() ! Status.Failure(new IllegalStateException(s"Changed ${jobnet.path}"))
      }

    case AddOrder(order) ⇒
      import order.nodeKey
      pathToJobnet.get(nodeKey.jobnetPath) match {
        case Some(jobnet) if jobnet isDefinedAt nodeKey.nodeId ⇒
          addOrder(order) map { case Completed ⇒ EmptyResponse } pipeTo sender()
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
      val whenResponded: Future[GetOrders.Response] = Future.sequence(
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

  private def addOrder(order: Order[Order.Idle]): Future[Completed] = {
    val orderEntry = orderRegister.getOrElseUpdate(order.id, {
      val actor = addOrderActor(order.id)
      OrderEntry(order.id, actor, order.jobnetPath, order.nodeId)
    })
    (orderEntry.actor ? OrderActor.Command.Attach(order)).mapTo[Completed]
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
        handleChangedOrderState(order)

      case _: OrderStepEnded ⇒
        for (jobNode ← nodeKeyToJobNodeOption(orderEntry.nodeKey);
             jobEntry ← jobRegister.get(jobNode.jobPath)) {
          jobEntry.queue -= orderId
        }

      case e: OrderNodeChanged ⇒
        orderEntry.nodeId = e.nodeId
        onOrderAvailable(orderEntry)

      case _ ⇒
    }
  }

  private def handleChangedOrderState(order: Order[Order.State]): Unit = {
    val orderEntry = orderRegister(order.id)
    import orderEntry.orderId
    order.state match {
      case Order.Scheduled(instant) if now < instant ⇒
        val timer = timerService.at(instant, name = orderId.string)
        timer onElapsed {
          self ! Internal.Due(orderId)
        }
        orderEntry.timer foreach timerService.cancel
        orderEntry.timer = Some(timer)

      case Order.Scheduled(_) | Order.StartNow ⇒
        onOrderAvailable(orderEntry)

      case Order.Waiting | Order.Detached/*???*/ ⇒
        orderEntry.nodeId = order.nodeId
        onOrderAvailable(orderEntry)

      case _ ⇒
    }
  }

  private def onOrderAvailable(orderEntry: OrderEntry): Unit =
    nodeKeyToNodeOption(orderEntry.nodeKey) match {
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
        nodeKeyToNodeOption(orderEntry.nodeKey) match {
          case Some(jobNode: Jobnet.JobNode) ⇒
            logger.trace(s"${orderEntry.orderId} is going to be processed by ${jobEntry.jobPath}")
            assert(jobNode.jobPath == jobEntry.jobPath)
            jobEntry.requestedOrderIds += orderId
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
      case Terminated(actorRef) if jobRegister isDefinedAt actorRef ⇒
        val jobPath = jobRegister.actorToKey(actorRef)
        val msg = s"Job Actor '${jobPath.string}' terminated unexpectedly"
        logger.warn(msg)
        jobRegister -= actorRef

      case Terminated(actorRef) if orderRegister isDefinedAt actorRef ⇒
        for (orderEntry ← orderRegister.remove(orderRegister.actorToKey(actorRef))) {
          logger.debug(s"Removing ${orderEntry.orderId}")
          orderEntry.timer foreach timerService.cancel
        }

      case _ ⇒
        super.unhandled(message)
    }

  private def nodeKeyToJobNodeOption(nodeKey: NodeKey): Option[Jobnet.JobNode] =
    nodeKeyToNodeOption(nodeKey) collect { case o: Jobnet.JobNode ⇒ o }

  private def nodeKeyToNodeOption(nodeKey: NodeKey): Option[Jobnet.Node] =
    pathToJobnet(nodeKey.jobnetPath).idToNode.get(nodeKey.nodeId)
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[Jobnet],
    Subtype[Order[Order.State]],
    Subtype[EventQueue.CompleteSnapshot])

  private val MyJournalMeta = new JsonJournalMeta(
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

  private class JobEntry(val jobPath: JobPath, val actor: ActorRef) {
    val queue = new OrderQueue
    val requestedOrderIds = mutable.Set[OrderId]()
    var waitingForOrder = false
  }

  private class OrderQueue {
    private var queue: mutable.ListBuffer[OrderId] = mutable.ListBuffer()
    private var inProcess = mutable.Set[OrderId]()

    def dequeue(): Option[OrderId] =
      for (orderId ← queue find { o ⇒ !inProcess(o) }) yield {
        inProcess += orderId
        orderId
      }

    def +=(orderId: OrderId) = queue += orderId

    def -=(orderId: OrderId) = {
      queue -= orderId
      inProcess -= orderId
    }
  }

  private case class OrderEntry(
    orderId: OrderId,
    actor: ActorRef,
    jobnetPath: JobnetPath,
    var nodeId: NodeId,
    var timer: Option[Timer[Unit]] = None)
  {
    def nodeKey = NodeKey(jobnetPath, nodeId)
  }
}
