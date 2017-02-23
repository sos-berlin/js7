package com.sos.scheduler.engine.agent.orderprocessing

import akka.actor.{ActorRef, Props, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.sos.scheduler.engine.agent.data.commandresponses.EmptyResponse
import com.sos.scheduler.engine.agent.data.commands.{AddJobNet, AddOrder, DetachOrder, GetOrder, GetOrderIds, GetOrders, OrderCommand}
import com.sos.scheduler.engine.agent.orderprocessing.AgentOrderKeeper._
import com.sos.scheduler.engine.agent.orderprocessing.KeyedEventJsonFormats.AgentKeyedEventJsonFormat
import com.sos.scheduler.engine.agent.orderprocessing.job.JobRunner
import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.common.akkautils.Akkas.encodeAsActorName
import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.{Timer, TimerService}
import com.sos.scheduler.engine.data.engine2.order.JobNet.{JobNode, Node}
import com.sos.scheduler.engine.data.engine2.order.JobnetEvent.JobnetAttached
import com.sos.scheduler.engine.data.engine2.order.OrderEvent.{OrderAttached, OrderNodeChanged, OrderStepEnded}
import com.sos.scheduler.engine.data.engine2.order.{JobChainPath, JobNet, JobPath, JobnetEvent, NodeId, NodeKey, Order, OrderEvent}
import com.sos.scheduler.engine.data.event.{EventId, KeyedEvent, Snapshot}
import com.sos.scheduler.engine.data.order.OrderId
import com.sos.scheduler.engine.shared.common.ActorRegister
import com.sos.scheduler.engine.shared.event.SnapshotKeyedEventBus
import com.sos.scheduler.engine.shared.event.journal.JsonJournalRecoverer.{RecoveringChanged, RecoveringDeleted, RecoveringForKnownKey, RecoveringForUnknownKey, RecoveringSnapshot}
import com.sos.scheduler.engine.shared.event.journal.{Journal, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
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
  keyedEventBus: SnapshotKeyedEventBus,
  eventIdGenerator: EventIdGenerator,
  timerService: TimerService)
extends KeyedEventJournalingActor[JobnetEvent] with Stash {

  import context.dispatcher

  protected val journalActor = context.actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val jobRegister = new ActorRegister[JobPath, JobEntry](_.actor)
  private val pathToJobnet = mutable.Map[JobChainPath, JobNet]()
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
          case RecoveringSnapshot(jobnet: JobNet) ⇒
            pathToJobnet += jobnet.path → jobnet

          case RecoveringSnapshot(order: Order[Order.State]) ⇒
            val actor = addOrderActor(order.id)
            orderRegister += order.id → OrderEntry(order.id, actor, order.jobChainPath, order.nodeId)
            journal.addActorForSnapshot(order, actor)

          case RecoveringSnapshot(snapshot: EventQueue.CompleteSnapshot) ⇒
            eventsForMaster ! snapshot  // TODO FinishRecovery for synchronization ?

          case RecoveringForUnknownKey(Snapshot(_, KeyedEvent(path: JobChainPath, event: JobnetEvent.JobnetAttached))) ⇒
            pathToJobnet += path → JobNet.fromJobnetAttached(path, event)

          case RecoveringForUnknownKey(eventSnapshot @ Snapshot(_, KeyedEvent(orderId: OrderId, event: OrderEvent.OrderAttached))) ⇒
            val actor = addOrderActor(orderId)
            orderRegister += orderId → OrderEntry(orderId, actor, event.nodeKey.jobChainPath, event.nodeKey.nodeId)
            journal.addActorForFirstEvent(eventSnapshot, actor)
            eventsForMaster ! eventSnapshot

          case RecoveringDeleted(eventSnapshot @ Snapshot(_, KeyedEvent(orderId: OrderId, OrderEvent.OrderDetached))) ⇒
            // OrderActor stops itself
            context.unwatch(orderRegister(orderId).actor)
            orderRegister -= orderId
            eventsForMaster ! eventSnapshot

          case RecoveringChanged(eventSnapshot @ Snapshot(_, KeyedEvent(_: OrderId, _: OrderEvent))) ⇒
            eventsForMaster ! eventSnapshot
        }
        journal.recoveredJournalingActors
      }
    journalActor ! Journal.Input.Start(recovered)
  }

  override def postStop() = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def receive = {
    case Journal.Output.Ready ⇒
      for (o ← orderRegister.values) o.actor ! OrderActor.Input.FinishRecovery  // Responds with OrderActor.Output.RecoveryFinished
      context.become(startable)
      logger.info(s"${pathToJobnet.size} Jobnets recovered, ${orderRegister.size} Orders recovered")
      unstashAll()

    case msg ⇒
      receiveJournalMessage.applyOrElse(msg, (_: Any) ⇒ stash())
  }

  private def startable: Receive = {
    case Input.Start(jobPathsAndActors) ⇒
      for ((jobPath, actorRef) ← jobPathsAndActors) {
        jobRegister += jobPath → new JobEntry(jobPath, actorRef)
        context.watch(actorRef)
      }
      context.become(ready)
      unstashAll()
      logger.info("Ready")

    case msg ⇒
      receiveJournalMessage.applyOrElse(msg, (_: Any) ⇒ stash())
  }

  private def ready: Receive = {
    case OrderActor.Output.RecoveryFinished(order) ⇒  // TODO Ist es gut, schon bereit zu sein, während noch die Aufträge ihre Wiederherstellung bestätigen? Was wenn währenddessen ein Kommando kommt?
      logger.info(s"Recovered: $order")
      handleChangedOrderState(order)

    case cmd: OrderCommand ⇒
      processOrderCommand(cmd)

    case OrderActor.Output.OrderChanged(order, event) if orderRegister contains order.id ⇒
      handleOrderEvent(order, event)

    case Internal.Due(orderId) if orderRegister isDefinedAt orderId ⇒
      val orderEntry = orderRegister(orderId)
      orderEntry.timer = None
      onOrderAvailable(orderEntry)

    case eventSnapshot @ Snapshot(_, KeyedEvent(orderId: OrderId, _: OrderEvent)) if orderRegister contains orderId ⇒
      eventsForMaster ! eventSnapshot

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      eventsForMaster.forward(EventQueue.Input.RequestEvents(after, timeout, limit, promise))

    case JobRunner.Output.ReadyForOrder if jobRegister isDefinedAt sender() ⇒
      tryStartStep(jobRegister(sender()))
  }

  private def processOrderCommand(cmd: OrderCommand) = cmd match {
    case AddJobNet(jobNet) ⇒
      pathToJobnet.get(jobNet.path) match {
        case None ⇒
          persist(KeyedEvent(JobnetAttached(jobNet.inputNodeId, jobNet.idToNode))(jobNet.path)) { _ ⇒
            pathToJobnet += jobNet.path → jobNet
            sender() ! EmptyResponse
          }
        case Some(`jobNet`) ⇒
          sender() ! EmptyResponse
        case Some(_) ⇒
          sender() ! Status.Failure(new IllegalStateException(s"Changed ${jobNet.path}"))
      }

    case AddOrder(order) ⇒
      import order.nodeKey
      pathToJobnet.get(nodeKey.jobChainPath) match {
        case Some(jobnet) if jobnet isDefinedAt nodeKey.nodeId ⇒
          addOrder(order) map { case Completed ⇒ EmptyResponse } pipeTo sender()
        case Some(_) ⇒
          sender() ! Status.Failure(new IllegalArgumentException(s"Unknown NodeId ${nodeKey.nodeId} in ${nodeKey.jobChainPath}"))
        case None ⇒
          sender() ! Status.Failure(new IllegalArgumentException(s"Unknown ${nodeKey.jobChainPath}"))
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
      OrderEntry(order.id, actor, order.jobChainPath, order.nodeId)
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
      case Some(node: JobNode) ⇒
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
          case Some(jobNode: JobNode) ⇒
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

  private def nodeKeyToJobNodeOption(nodeKey: NodeKey): Option[JobNode] =
    nodeKeyToNodeOption(nodeKey) collect { case o: JobNode ⇒ o }

  private def nodeKeyToNodeOption(nodeKey: NodeKey): Option[Node] =
    pathToJobnet(nodeKey.jobChainPath).idToNode.get(nodeKey.nodeId)
}

object AgentOrderKeeper {
  private val logger = Logger(getClass)

  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[JobNet],
    Subtype[Order[Order.State]],
    Subtype[EventQueue.CompleteSnapshot])

  private val MyJournalMeta = new JsonJournalMeta(
    SnapshotJsonFormat,
    AgentKeyedEventJsonFormat,
    snapshotToKey = {
      case jobnet: JobNet ⇒ jobnet.path
      case order: Order[_] ⇒ order.id
    },
    isDeletedEvent = Set(OrderEvent.OrderDetached))

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
    jobChainPath: JobChainPath,
    var nodeId: NodeId,
    var timer: Option[Timer[Unit]] = None)
  {
    def nodeKey = NodeKey(jobChainPath, nodeId)
  }
}
