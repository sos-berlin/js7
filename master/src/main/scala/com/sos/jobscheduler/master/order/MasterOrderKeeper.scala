package com.sos.scheduler.engine.master.order

import akka.Done
import akka.actor.{ActorRef, OneForOneStrategy, Props, Stash, Status, SupervisorStrategy}
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.common.akkautils.Akkas.encodeAsActorName
import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.EventCollector
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.xmls.FileSource
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.scheduler.engine.data.engine2.agent.AgentPath
import com.sos.scheduler.engine.data.engine2.order.JobNet.{EndNode, JobNode}
import com.sos.scheduler.engine.data.engine2.order.OrderEvent.OrderAdded
import com.sos.scheduler.engine.data.engine2.order.{JobChainPath, JobNet, Order, OrderEvent}
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Snapshot}
import com.sos.scheduler.engine.data.order.OrderId
import com.sos.scheduler.engine.master.KeyedEventJsonFormats.MasterKeyedEventJsonFormat
import com.sos.scheduler.engine.master.command.MasterCommand
import com.sos.scheduler.engine.master.configuration.MasterConfiguration
import com.sos.scheduler.engine.master.order.MasterOrderKeeper._
import com.sos.scheduler.engine.master.order.agent.{AgentDriver, AgentParser}
import com.sos.scheduler.engine.master.{AgentEventId, AgentEventIdEvent}
import com.sos.scheduler.engine.shared.common.ActorRegister
import com.sos.scheduler.engine.shared.event.SnapshotKeyedEventBus
import com.sos.scheduler.engine.shared.event.journal.{Journal, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
import com.sos.scheduler.engine.shared.filebased.TypedPathDirectoryWalker.forEachTypedFile
import scala.collection.mutable
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class MasterOrderKeeper(masterConfiguration: MasterConfiguration)
  (implicit
    timerService: TimerService,
    eventIdGenerator: EventIdGenerator,
    eventCollector: EventCollector,
    keyedEventBus: SnapshotKeyedEventBus)
extends KeyedEventJournalingActor[Event]
with Stash {

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0) {
    case _ ⇒ SupervisorStrategy.Stop
  }

  import context.{become, dispatcher}
  intelliJuseImports(dispatcher)

  private val journalFile = masterConfiguration.stateDirectoryOption.get/*optional ???*/ / "journal"
  private val agentRegister = new ActorRegister[AgentPath, AgentEntry](_.actor)
  private val pathToJobnet = mutable.Map[JobChainPath, JobNet]()
  private val orders = mutable.Map[OrderId, OrderEntry]()
  private val lastRecoveredOrderEvents = mutable.Map[OrderId, OrderEvent]()
  private var detachingSuspended = false
  protected val journalActor = context.actorOf(
    Props {
      new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = masterConfiguration.journalSyncOnCommit, eventIdGenerator, keyedEventBus)
    },
    "Journal")

  for (dir ← masterConfiguration.liveDirectoryOption) {
    forEachTypedFile(dir, Set(JobChainPath, AgentPath)) {
      case (file, jobChainPath: JobChainPath) ⇒
        logger.info(s"Adding $jobChainPath")
        val jobNet = autoClosing(new FileSource(file)) { src ⇒
          JobNetParser.parseXml(jobChainPath, src)
        }
        pathToJobnet += jobChainPath → jobNet

      case (file, agentPath: AgentPath) ⇒
        logger.info(s"Adding $agentPath")
        val agent = autoClosing(new FileSource(file)) { src ⇒
          AgentParser.parseXml(agentPath, src)
        }
        val actor = context.actorOf(
          Props { new AgentDriver(agent.path, agent.uri) },
          name = encodeAsActorName("Agent-" + agentPath.withoutStartingSlash))
        agentRegister += agentPath → AgentEntry(agentPath, actor)
    }
  }

  protected def snapshots = Future.successful((
      (for (entry ← agentRegister.values) yield AgentEventId(entry.agentPath, entry.lastAgentEventId)) ++
      //pathToJobnet.values ++
      (for (entry ← orders.values) yield entry.order)
    ).toVector)

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  private def recover() = {
    val recovered =
      autoClosing(new JsonJournalRecoverer(MyJournalMeta, journalFile)) { journal ⇒
        import JsonJournalRecoverer._
        for (recovered ← journal) (recovered: @unchecked) match {
          case RecoveringSnapshot(order: Order[Order.State]) ⇒
            orders += order.id → OrderEntry(order)

          case RecoveringSnapshot(AgentEventId(agentPath, eventId)) ⇒
            agentRegister(agentPath).lastAgentEventId = eventId
            //journal.addActorForSnapshot(snapshot, agentRegister(agentPath).actor)

          case RecoveringForUnknownKey(Snapshot(_, KeyedEvent(orderId: OrderId, event: OrderEvent))) ⇒
            event match {
              case event: OrderEvent.OrderAdded ⇒
                addOrder(orderId, event)
              case _ ⇒
                orders(orderId).update(event)
            }
            lastRecoveredOrderEvents += orderId → event

          case RecoveringForUnknownKey(Snapshot(_, KeyedEvent(agentPath: AgentPath, AgentEventIdEvent(agentEventId)))) ⇒
            agentRegister(agentPath).lastAgentEventId = agentEventId
        }
        journal.recoveredJournalingActors
      }

    journalActor ! Journal.Input.Start(recovered)
    //journalActor ! Journal.Input.Start(RecoveredJournalingActors(
    //  recovered.keyToJournalingActor ++ ((pathToJobnet.keys ++ orders.keys ++ agentRegister.keys) map { _ → self })))   // Separate message ???
  }

  override def postStop(): Unit = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def receive: Receive = {
    case Journal.Output.Ready ⇒
      for (o ← orders.values) logger.info(s"Recovered: ${o.order}")
      for (agentEntry ← agentRegister.values) {
        val orderIds = orders.values collect {
          case orderEntry if orderEntry.order.agentPathOption contains agentEntry.agentPath ⇒
            orderEntry.orderId
        }
        agentEntry.actor ! AgentDriver.Input.Recover(
          lastAgentEventId = agentEntry.lastAgentEventId,
          orderIds = orderIds.toImmutableSeq)
      }
      becomeWaitingForAgentDriverRecovery(agentRegister.size)

    case msg ⇒
      receiveJournalMessage.applyOrElse(msg, (_: Any) ⇒ stash())
  }

  private def becomeWaitingForAgentDriverRecovery(remaining: Int): Unit = {
    if (remaining > 0) {
      context.become {
        case AgentDriver.Output.Recovered ⇒
          becomeWaitingForAgentDriverRecovery(remaining - 1)

        case msg ⇒
          receiveJournalMessage.applyOrElse(msg, (_: Any) ⇒ stash())
      }
    } else {
      for (order ← orders.valuesIterator map { _.order }) {
        order.state match {
          case Order.Scheduled(at) ⇒
          case Order.StartNow ⇒
          case Order.Waiting ⇒
          case Order.Ready ⇒
          case Order.InProcess ⇒
          case Order.Detached ⇒
            //val detachedOrder = order.asInstanceOf[Order[Order.Detached.type]]
            //// TODO Duplicate code
            //for (jobNet ← pathToJobnet.get(detachedOrder.nodeKey.jobChainPath);
            //     jobNode ← jobNet.idToNode.get(detachedOrder.nodeKey.nodeId) collect { case o: JobNode ⇒ o }) {
            //  tryAttachOrderToAgent(detachedOrder, jobNet, jobNode.agentPath)
            //}
          case Order.Finished ⇒
        }
      }
      for ((orderId, event) ← lastRecoveredOrderEvents) {
        handleOrderEvent(orderId, event)  // Repeat last event induced action. Besser den Zustand als das letzte Ereignis prüfen ??? Vielleicht muss auch die Aktion ein länger zurückliegenden Ereignisses wiederholt werden.
      }
      lastRecoveredOrderEvents.clear()
      become(ready)
      logger.info(s"Ready, ${orders.size} Orders recovered")
      for (o ← agentRegister.values) {
        o.actor ! AgentDriver.Input.Start
      }
      unstashAll()
    }
  }

  private def ready: Receive = {
    case MasterCommand.AddOrderIfNew(order) ⇒
      orders.get(order.id) match {
        case Some(_) ⇒
          logger.debug(s"Discarding duplicate AddOrderIfNew: ${order.id}")
          sender() ! MasterCommand.Response.Accepted  //Status.Failure(new IllegalStateException(s"Duplicate OrderId '${order.id}'"))
        case None ⇒
          if (!pathToJobnet.isDefinedAt(order.nodeKey.jobChainPath)) {
            sender() ! Status.Failure(new NoSuchElementException(s"Unknown JobNet '${order.nodeKey.jobChainPath.string}'"))
          } else {
            persist(KeyedEvent(OrderEvent.OrderAdded(order.nodeKey, order.state, order.variables, order.outcome))(order.id)) { _ ⇒
              sender() ! MasterCommand.Response.Accepted
            }
          }
      }

    case Command.Get(orderId) ⇒
      sender() ! (orders.get(orderId) map { _.order })

    case Command.Remove(orderId) ⇒
      orders.get(orderId) match {
        case None ⇒ sender() ! Status.Failure(new NoSuchElementException(s"Unknown $orderId"))
        case Some(orderEntry) ⇒
          if (orderEntry.toBeRemoved)
            sender() ! Done
          else {
            orderEntry.toBeRemoved = true
            orderEntry.order.agentPathOption match {
              case Some(agentPath) ⇒
                sender() ! Status.Failure(new IllegalStateException(s"Order cannot be removed because it is attached to Agent '$agentPath'"))  // ???
                //(agents(agentPath) ? AgentDriver.Input.DetachOrder(orderId)).mapTo[EmptyResponse.type]
                //  .pipeTo(sender())
              case None ⇒
                //orderEntry.order = orderEntry.order.update(OrderRemoved)  // TOOD Persist
                sender() ! Done
            }
          }
      }

    case msg @ AgentDriver.Output.EventFromAgent(Snapshot(agentEventId, KeyedEvent(orderId: OrderId, event: OrderEvent))) ⇒
      val agentEntry = agentRegister(sender())
      import agentEntry.agentPath
      if (orders contains orderId) {
        val ownEvent = event match {
          case _: OrderEvent.OrderAttached ⇒ OrderEvent.OrderMovedToAgent(agentPath)  // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
          case OrderEvent.OrderDetached ⇒ OrderEvent.OrderMovedToMaster
          case _ ⇒ event
        }
        persist(KeyedEvent(ownEvent)(orderId)) { _ ⇒ }
        persist(KeyedEvent(AgentEventIdEvent(agentEventId))(agentPath)) { e ⇒
          agentEntry.lastAgentEventId = e.eventId
        }
      } else {
        logger.warn(s"Event for unknown $orderId received from $agentPath: $msg")
      }

    case msg @ AgentDriver.Output.OrderDetached(orderId) ⇒
      val agentPath = agentRegister.actorToKey(sender())
      if (orders contains orderId) {
        persist(KeyedEvent(OrderEvent.OrderMovedToMaster)(orderId)) { _ ⇒ }
      } else {
        logger.warn(s"Event for unknown $orderId received from $agentPath: $msg")
      }

    case msg @ Journal.Output.SerializationFailure(throwable) ⇒
      logger.error(msg.toString, throwable) // Ignore this ???

    case Snapshot(_, keyedEvent: AnyKeyedEvent) ⇒
      keyedEvent match {
        case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
          handleOrderEvent(orderId, event)
        case _ ⇒
      }

    case Input.SuspendDetaching ⇒
      if (!detachingSuspended) {
        logger.warn("SuspendDetaching")
        detachingSuspended = true
      }

    case Input.ContinueDetaching ⇒
      if (detachingSuspended) {
        logger.info("ContinueDetaching")
        detachingSuspended = false
        unstashAll()
      }
  }

  private def addOrder(orderId: OrderId, event: OrderEvent.OrderAdded): OrderEntry = {
    val e = OrderEntry(Order.fromOrderAdded(orderId, event))
    orders += orderId → e
    e
  }

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit = {
    event match {
      case event: OrderAdded ⇒
        addOrder(orderId, event)
        for (jobNet ← pathToJobnet.get(event.nodeKey.jobChainPath);
             jobNode ← jobNet.idToNode.get(event.nodeKey.nodeId) collect { case o: JobNode ⇒ o }) {
          val order = Order[Order.Idle](orderId, event.nodeKey, event.state, event.variables, event.outcome)
          tryAttachOrderToAgent(order, jobNet, jobNode.agentPath)
        }

      case event: OrderEvent ⇒
        val orderEntry = orders(orderId)
        orderEntry.update(event)  // May crash !!!
        for (orderEntry ← orders.get(orderId)) {
          event match {
            case OrderEvent.OrderReady if detachingSuspended ⇒
              stash()

            case OrderEvent.OrderReady ⇒
              for (path ← orderEntry.order.agentPathOption)
                agentRegister(path).actor ! AgentDriver.Input.DetachOrder(orderId)

            case OrderEvent.OrderMovedToMaster/*was OrderDetached*/ ⇒
              moveAhead(orderId)

            case _ ⇒
          }
        }
    }
  }

  private def moveAhead(orderId: OrderId): Unit = {
    val orderEntry = orders(orderId)
    for (jobNet ← pathToJobnet.get(orderEntry.order.jobChainPath);
         node ← jobNet.idToNode.get(orderEntry.order.nodeId)) {
      node match {
        case node: JobNode ⇒
          val idleOrder = orderEntry.order.castState[Order.Idle]  // ???
          tryAttachOrderToAgent(idleOrder, jobNet, node.agentPath)
        case _: EndNode ⇒
          persist(KeyedEvent(OrderEvent.OrderFinished)(orderId)) { eventSnapshot ⇒
            logger.info(eventSnapshot.toString)
          }
      }
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.Idle], jobNet: JobNet, agentPath: AgentPath): Unit = {
    for (agentEntry ← agentRegister.get(agentPath)) {
      agentEntry.actor ! AgentDriver.Input.AttachOrder(order, jobNet.reduceForAgent(agentPath))
    }
  }
}

object MasterOrderKeeper {
  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[Order[Order.State]],
    Subtype[AgentEventId])
  //Subtype[JobNet])

  private val MyJournalMeta = new JsonJournalMeta(
    SnapshotJsonFormat,
    MasterKeyedEventJsonFormat,
    snapshotToKey = {
      case order: Order[_] ⇒ order.id
    },
    isDeletedEvent = Set(/*OrderEvent.OrderRemoved fehlt*/))

  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    case object SuspendDetaching extends Input    // For testing
    case object ContinueDetaching extends Input   // For testing
  }

  sealed trait Command
  object Command {
    final case class Get(orderId: OrderId) extends Command
    final case class Remove(orderId: OrderId) extends Command
  }

  private case class AgentEntry(
    agentPath: AgentPath,
    actor: ActorRef,
    var lastAgentEventId: EventId = EventId.BeforeFirst)

  private case class OrderEntry(
    var order: Order[Order.State],
    var toBeRemoved: Boolean = false)
  {
    def orderId = order.id

    def update(event: OrderEvent): Unit = {
      order = order.update(event)
    }
  }
}
