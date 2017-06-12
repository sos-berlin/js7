package com.sos.jobscheduler.master.order

import akka.Done
import akka.actor.{ActorRef, OneForOneStrategy, Props, Stash, Status, SupervisorStrategy}
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.FileSource
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.{Jobnet, JobnetPath}
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.KeyedEventJsonFormats.MasterKeyedEventJsonFormat
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.order.MasterOrderKeeper._
import com.sos.jobscheduler.master.order.agent.{AgentDriver, AgentXmlParser}
import com.sos.jobscheduler.master.{AgentEventId, AgentEventIdEvent}
import com.sos.jobscheduler.shared.common.ActorRegister
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
import com.sos.jobscheduler.shared.filebased.TypedPathDirectoryWalker.forEachTypedFile
import java.time.Duration
import scala.collection.mutable
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class MasterOrderKeeper(
  masterConfiguration: MasterConfiguration,
  scheduledOrderGeneratorKeeper: ScheduledOrderGeneratorKeeper)
  (implicit
    timerService: TimerService,
    eventIdGenerator: EventIdGenerator,
    eventCollector: EventCollector,
    keyedEventBus: StampedKeyedEventBus)
extends KeyedEventJournalingActor[Event]
with Stash {

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0) {
    case _ ⇒ SupervisorStrategy.Stop
  }

  import context.{become, dispatcher}
  intelliJuseImports(dispatcher)

  private val journalFile = masterConfiguration.stateDirectory / "journal"
  private val agentRegister = new ActorRegister[AgentPath, AgentEntry](_.actor)
  private val pathToJobnet = mutable.Map[JobnetPath, Jobnet]()
  private val orderRegister = mutable.Map[OrderId, OrderEntry]()
  private val lastRecoveredOrderEvents = mutable.Map[OrderId, OrderEvent]()
  private var detachingSuspended = false
  protected val journalActor = context.actorOf(
    Props {
      new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = masterConfiguration.journalSyncOnCommit, eventIdGenerator, keyedEventBus)
    },
    "Journal")
  private val orderScheduleGenerator = context.actorOf(
    Props { new OrderScheduleGenerator(journalActor = journalActor, masterOrderKeeper = self, scheduledOrderGeneratorKeeper)},
    "OrderScheduleGenerator"
  )

  for (dir ← masterConfiguration.liveDirectoryOption) {
    forEachTypedFile(dir, Set(JobnetPath, AgentPath)) {
      case (file, jobnetPath: JobnetPath) ⇒
        logger.info(s"Adding $jobnetPath")
        val jobnet = autoClosing(new FileSource(file)) { src ⇒
          JobnetXmlParser.parseXml(jobnetPath, src)
        }
        pathToJobnet += jobnetPath → jobnet

      case (file, agentPath: AgentPath) ⇒
        logger.info(s"Adding $agentPath")
        val agent = autoClosing(new FileSource(file)) { src ⇒
          AgentXmlParser.parseXml(agentPath, src)
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
      (for (entry ← orderRegister.values) yield entry.order)
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
          case RecoveringSnapshot(o: OrderScheduleEndedAt) ⇒
            journal.recoverActorForSnapshot(o, orderScheduleGenerator)

          case RecoveringForUnknownKey(stamped @ Stamped(_, KeyedEvent(_: NoKey.type, _: OrderScheduleEvent))) ⇒
            journal.recoverActorForFirstEvent(stamped, orderScheduleGenerator)

          case RecoveringChanged(Stamped(_, KeyedEvent(_: NoKey.type, _: OrderScheduleEvent))) ⇒

          case RecoveringSnapshot(order: Order[Order.State]) ⇒
          orderRegister += order.id → OrderEntry(order)

          case RecoveringSnapshot(AgentEventId(agentPath, eventId)) ⇒
          agentRegister(agentPath).lastAgentEventId = eventId
          //journal.recoverActorForSnapshot(snapshot, agentRegister(agentPath).actor)

          case RecoveringForUnknownKey(Stamped(_, KeyedEvent(orderId: OrderId, event: OrderEvent))) ⇒
            event match {
              case event: OrderEvent.OrderAdded ⇒
                onOrderAdded(orderId, event)
              case _ ⇒
                orderRegister(orderId).update(event)
            }
            lastRecoveredOrderEvents += orderId → event

          case RecoveringForUnknownKey(Stamped(_, KeyedEvent(agentPath: AgentPath, AgentEventIdEvent(agentEventId)))) ⇒
            agentRegister(agentPath).lastAgentEventId = agentEventId
        }
        journal.recoveredJournalingActors
      }

    journalActor ! JsonJournalActor.Input.Start(recovered)
    //journalActor ! JsonJournalActor.Input.Start(RecoveredJournalingActors(
    //  recovered.keyToJournalingActor ++ ((pathToJobnet.keys ++ orders.keys ++ agentRegister.keys) map { _ → self })))   // Separate message ???
  }

  override def postStop(): Unit = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  def receive = journaling orElse {
    case JsonJournalActor.Output.Ready ⇒
      //for (o ← orderRegister.values) logger.info(s"Recovered: ${o.order}")
      for (agentEntry ← agentRegister.values) {
        val orderIds = orderRegister.values collect {
          case orderEntry if orderEntry.order.agentPathOption contains agentEntry.agentPath ⇒
            orderEntry.orderId
        }
        agentEntry.actor ! AgentDriver.Input.Recover(
          lastAgentEventId = agentEntry.lastAgentEventId,
          orderIds = orderIds.toImmutableSeq)
      }
      becomeWaitingForAgentDriverRecovery(agentRegister.size)

    case _ ⇒
      stash()
  }

  private def becomeWaitingForAgentDriverRecovery(remaining: Int): Unit = {
    if (remaining > 0) {
      become(journaling orElse {
        case AgentDriver.Output.Recovered ⇒
          becomeWaitingForAgentDriverRecovery(remaining - 1)

        case _ ⇒
          stash()
      })
    } else {
      for (order ← orderRegister.valuesIterator map { _.order }) {
        order.state match {
          case Order.Scheduled(at) ⇒
          case Order.StartNow ⇒
          case Order.Waiting ⇒
          case Order.Ready ⇒
          case Order.InProcess ⇒
          case Order.Detached ⇒
            //val detachedOrder = order.asInstanceOf[Order[Order.Detached.type]]
            //// TODO Duplicate code
            //for (jobnet ← pathToJobnet.get(detachedOrder.nodeKey.jobnetPath);
            //     jobNode ← jobnet.idToNode.get(detachedOrder.nodeKey.nodeId) collect { case o: JobNode ⇒ o }) {
            //  tryAttachOrderToAgent(detachedOrder, jobnet, jobNode.agentPath)
            //}
          case Order.Finished ⇒
        }
      }
      for ((orderId, event) ← lastRecoveredOrderEvents) {
        handleOrderEvent(orderId, event)  // Repeat last event induced action. Besser den Zustand als das letzte Ereignis prüfen ??? Vielleicht muss auch die Aktion ein länger zurückliegenden Ereignisses wiederholt werden.
      }
      lastRecoveredOrderEvents.clear()
      become(ready)
      logger.info(s"Ready, ${orderRegister.size} Orders recovered")
      for (o ← agentRegister.values) {
        o.actor ! AgentDriver.Input.Start
      }
      unstashAll()
    }
  }

  private def ready: Receive = journaling orElse {
    case command: MasterCommand ⇒
      executeMasterCommand(command)

    case Command.AddOrderSchedule(orders) ⇒
      for (order ← orders) {
        val logMsg = s"Order scheduled for ${order.state.at}: '${order.id}'"
        orderRegister.get(order.id) match {
          case Some(_) ⇒
            logger.info(s"$logMsg is duplicate and discarded")
          case None if !pathToJobnet.isDefinedAt(order.nodeKey.jobnetPath) ⇒
            logger.error(s"$logMsg: Unknown '${order.nodeKey.jobnetPath}'")
          case _ ⇒
            persistAsync(KeyedEvent(OrderEvent.OrderAdded(order.nodeKey, order.state, order.variables, order.outcome))(order.id)) { _ ⇒
              logger.info(logMsg)
            }
        }
      }
      deferAsync {
        sender() ! Done
      }

    case Command.GetOrder(orderId) ⇒
      sender() ! (orderRegister.get(orderId) map { _.order })

    case Command.GetOrders ⇒
      sender() ! eventIdGenerator.stamp((orderRegister.values map { _.order }).toVector: Vector[Order[Order.State]])

    case Command.Remove(orderId) ⇒
      orderRegister.get(orderId) match {
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

    case msg @ AgentDriver.Output.EventFromAgent(Stamped(agentEventId, KeyedEvent(orderId: OrderId, event: OrderEvent))) ⇒
      val agentEntry = agentRegister(sender())
      import agentEntry.agentPath
      if (orderRegister contains orderId) {
        val ownEvent = event match {
          case _: OrderEvent.OrderAttached ⇒ OrderEvent.OrderMovedToAgent(agentPath)  // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
          case OrderEvent.OrderDetached ⇒ OrderEvent.OrderMovedToMaster
          case _ ⇒ event
        }
        persistAsync(KeyedEvent(ownEvent)(orderId)) { _ ⇒ }
        persistAsync(KeyedEvent(AgentEventIdEvent(agentEventId))(agentPath)) { e ⇒
          agentEntry.lastAgentEventId = e.eventId
        }
      } else {
        logger.warn(s"Event for unknown $orderId received from $agentPath: $msg")
      }

    case msg @ AgentDriver.Output.OrderDetached(orderId) ⇒
      val agentPath = agentRegister.actorToKey(sender())
      if (orderRegister contains orderId) {
        persistAsync(KeyedEvent(OrderEvent.OrderMovedToMaster)(orderId)) { _ ⇒ }
      } else {
        logger.warn(s"Event for unknown $orderId received from $agentPath: $msg")
      }

    case msg @ JsonJournalActor.Output.SerializationFailure(throwable) ⇒
      logger.error(msg.toString, throwable) // Ignore this ???

    case Stamped(_, keyedEvent: AnyKeyedEvent) ⇒
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

    case Internal.Execute(callback) ⇒
      callback()
  }

  def executeMasterCommand(command: MasterCommand): Unit = command match {
    case MasterCommand.ScheduleOrdersEvery(every) ⇒
      orderScheduleGenerator ! OrderScheduleGenerator.Input.ScheduleEvery(every)
      sender() ! MasterCommand.Response.Accepted

    case MasterCommand.AddOrderIfNew(order) ⇒
      orderRegister.get(order.id) match {
        case None if pathToJobnet.isDefinedAt(order.nodeKey.jobnetPath) ⇒
          persistAsync(KeyedEvent(OrderEvent.OrderAdded(order.nodeKey, order.state, order.variables, order.outcome))(order.id)) { _ ⇒
            sender() ! MasterCommand.Response.Accepted
          }
        case None if !pathToJobnet.isDefinedAt(order.nodeKey.jobnetPath) ⇒
          sender() ! Status.Failure(new NoSuchElementException(s"Unknown Jobnet '${order.nodeKey.jobnetPath.string}'"))
        case Some(_) ⇒
          logger.debug(s"Discarding duplicate AddOrderIfNew: ${order.id}")
          sender() ! MasterCommand.Response.Accepted //Status.Failure(new IllegalStateException(s"Duplicate OrderId '${order.id}'"))
      }
  }

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit = {
    event match {
      case event: OrderAdded ⇒
        onOrderAdded(orderId, event)
        for (jobnet ← pathToJobnet.get(event.nodeKey.jobnetPath);
             jobNode ← jobnet.idToNode.get(event.nodeKey.nodeId) collect { case o: Jobnet.JobNode ⇒ o }) {
          val order = Order[Order.Idle](orderId, event.nodeKey, event.state, event.variables, event.outcome)
          tryAttachOrderToAgent(order, jobnet, jobNode.agentPath)
        }

      case event: OrderEvent ⇒
        val orderEntry = orderRegister(orderId)
        orderEntry.update(event)  // May crash !!!
        for (orderEntry ← orderRegister.get(orderId)) {
          event match {
            case _: OrderEvent.OrderReady.type if detachingSuspended ⇒
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

  private def onOrderAdded(orderId: OrderId, event: OrderEvent.OrderAdded): Unit = {
    orderRegister += orderId → OrderEntry(Order.fromOrderAdded(orderId, event))
  }

  private def moveAhead(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    for (jobnet ← pathToJobnet.get(orderEntry.order.jobnetPath);
         node ← jobnet.idToNode.get(orderEntry.order.nodeId)) {
      node match {
        case node: Jobnet.JobNode ⇒
          val idleOrder = orderEntry.order.castState[Order.Idle]  // ???
          tryAttachOrderToAgent(idleOrder, jobnet, node.agentPath)
        case _: Jobnet.EndNode ⇒
          persistAsync(KeyedEvent(OrderEvent.OrderFinished)(orderId)) { stamped ⇒
            logger.info(stamped.toString)
          }
      }
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.Idle], jobnet: Jobnet, agentPath: AgentPath): Unit = {
    for (agentEntry ← agentRegister.get(agentPath)) {
      agentEntry.actor ! AgentDriver.Input.AttachOrder(order, jobnet.reduceForAgent(agentPath))
    }
  }
}

object MasterOrderKeeper {
  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[OrderScheduleEndedAt],
    Subtype[Order[Order.State]],
    Subtype[AgentEventId])
  //Subtype[Jobnet])

  private val MyJournalMeta = new JsonJournalMeta(
      SnapshotJsonFormat,
      MasterKeyedEventJsonFormat,
      snapshotToKey = {
        case o: Order[_] ⇒ o.id
        case _: OrderScheduleEndedAt ⇒ classOf[OrderScheduleEndedAt]
      },
      isDeletedEvent = Set(/*OrderEvent.OrderRemoved fehlt*/))
    with GzipCompression

  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    case object SuspendDetaching extends Input    // For testing
    case object ContinueDetaching extends Input   // For testing
  }

  sealed trait Command
  object Command {
    final case class AddOrderSchedule(orders: Seq[Order[Order.Scheduled]]) extends Command
    final case class GetOrder(orderId: OrderId) extends Command
    final case object GetOrders extends Command
    final case class Remove(orderId: OrderId) extends Command
  }

  private object Internal {
    final case class Execute(callback: () ⇒ Unit)
    final case class GenerateNextOrders(every: Duration)
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
