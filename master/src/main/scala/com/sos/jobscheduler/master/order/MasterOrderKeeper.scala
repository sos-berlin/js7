package com.sos.jobscheduler.master.order

import akka.Done
import akka.actor.{ActorRef, Props, Stash, Status, Terminated}
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.xmls.FileSource
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.{Jobnet, JobnetPath}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderStdWritten}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.master.KeyedEventJsonFormats.MasterKeyedEventJsonFormat
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.order.MasterOrderKeeper._
import com.sos.jobscheduler.master.order.agent.{AgentDriver, AgentXmlParser}
import com.sos.jobscheduler.master.{AgentEventId, AgentEventIdEvent}
import com.sos.jobscheduler.shared.common.ActorRegister
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.startJournalAndFinishRecovery
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor, RecoveredJournalingActors}
import com.sos.jobscheduler.shared.filebased.TypedPathDirectoryWalker.forEachTypedFile
import java.time.Duration
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.control.NonFatal

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

  override val supervisorStrategy = SupervisorStrategies.escalate

  import context.{become, dispatcher}
  intelliJuseImports(dispatcher)

  private val journalFile = masterConfiguration.stateDirectory / "journal"
  private val agentRegister = new AgentRegister
  private val pathToJobnet = mutable.Map[JobnetPath, Jobnet]()
  private val orderRegister = mutable.Map[OrderId, OrderEntry]()
  private var detachingSuspended = false
  protected val journalActor = context.watch(context.actorOf(
    Props {
      new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = masterConfiguration.journalSyncOnCommit, eventIdGenerator, keyedEventBus)
    },
    "Journal"))
  private val orderScheduleGenerator = context.actorOf(
    Props { new OrderScheduleGenerator(journalActor = journalActor, masterOrderKeeper = self, scheduledOrderGeneratorKeeper)},
    "OrderScheduleGenerator"
  )
  private var terminating = false

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
        agentRegister.insert(agentPath → AgentEntry(agentPath, actor))
    }
  }

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  override def postStop() = {
    keyedEventBus.unsubscribe(self)
    super.postStop()
  }

  protected def snapshots = Future.successful(
    (for (entry ← agentRegister.values) yield AgentEventId(entry.agentPath, entry.lastAgentEventId)) ++
      //??? pathToJobnet.values ++
      (orderRegister.values map { _ .order }))

  private def recover() = {
    val recoverer = new MasterJournalRecoverer(journalFile = journalFile, orderScheduleGenerator = orderScheduleGenerator)
    recoverer.recoverAll()
    for (order ← recoverer.orders) {
      orderRegister.insert(order.id → OrderEntry(order))
    }
    for ((agentPath, eventId) ← recoverer.agentToEventId) {
      agentRegister(agentPath).lastAgentEventId = eventId
    }
    startJournalAndFinishRecovery(journalActor = journalActor,
      RecoveredJournalingActors(Map(OrderScheduleGenerator.Key → orderScheduleGenerator)))
  }

  def receive = journaling orElse {
    case JsonJournalRecoverer.Output.JournalIsReady ⇒
      for (agentEntry ← agentRegister.values) {
        agentEntry.actor ! AgentDriver.Input.Start(lastAgentEventId = agentEntry.lastAgentEventId)
      }
      orderRegister.valuesIterator map { _.order } foreach handleRecoveredOrder
      logger.info(s"${orderRegister.size} Orders recovered, ready")
      become(ready)
      unstashAll()

    case _ ⇒ stash()
  }

  private def handleRecoveredOrder(order: Order[Order.State]): Unit =
    order.state match {
      case Order.Ready ⇒ detachOrderFromAgent(order.id)
      case Order.Detached ⇒ tryAttachOrderToAgent(order.castState[Order.Detached.type])
      case _ ⇒
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
            persistAsync(KeyedEvent(OrderAdded(order.nodeKey, order.state, order.variables, order.outcome))(order.id)) { _ ⇒
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
                //orderEntry.order = orderEntry.order.update(OrderRemoved)  // TODO Persist
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
          case _ ⇒ event
        }
        persistAsync(KeyedEvent(ownEvent)(orderId)) { _ ⇒ }
        persistAsync(KeyedEvent(AgentEventIdEvent(agentEventId))(agentPath)) { e ⇒
          agentEntry.lastAgentEventId = e.eventId
        }
      } else {
        logger.warn(s"Event for unknown $orderId received from $agentPath: $msg")
      }

    case AgentDriver.Output.OrderDetached(orderId) if orderRegister contains orderId⇒
      persistAsync(KeyedEvent(OrderEvent.OrderMovedToMaster)(orderId)) { _ ⇒ }

    case msg @ JsonJournalActor.Output.SerializationFailure(throwable) ⇒
      logger.error(msg.toString, throwable)
      // Ignore this ???

    case Stamped(_, keyedEvent: AnyKeyedEvent) ⇒
      keyedEvent match {
        case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
          try handleOrderEvent(orderId, event)
          catch { case NonFatal(t) ⇒
            logger.error(s"$keyedEvent: $t", t)
            for (o ← orderRegister.get(orderId)) logger.error(o.order.toString)
          }
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

    case Terminated(`journalActor`) if terminating ⇒
      logger.info("Stop")
      context.stop(self)
  }

  def executeMasterCommand(command: MasterCommand): Unit = command match {
    case MasterCommand.ScheduleOrdersEvery(every) ⇒
      orderScheduleGenerator ! OrderScheduleGenerator.Input.ScheduleEvery(every)
      sender() ! MasterCommand.Response.Accepted

    case MasterCommand.AddOrderIfNew(order) ⇒
      orderRegister.get(order.id) match {
        case None if pathToJobnet.isDefinedAt(order.nodeKey.jobnetPath) ⇒
          persistAsync(KeyedEvent(OrderAdded(order.nodeKey, order.state, order.variables, order.outcome))(order.id)) { _ ⇒
            sender() ! MasterCommand.Response.Accepted
          }
        case None if !pathToJobnet.isDefinedAt(order.nodeKey.jobnetPath) ⇒
          sender() ! Status.Failure(new NoSuchElementException(s"Unknown Jobnet '${order.nodeKey.jobnetPath.string}'"))
        case Some(_) ⇒
          logger.debug(s"Discarding duplicate AddOrderIfNew: ${order.id}")
          sender() ! MasterCommand.Response.Accepted //Status.Failure(new IllegalStateException(s"Duplicate OrderId '${order.taskId}'"))
      }

    case MasterCommand.Terminate ⇒
      terminating = true
      journalActor ! JsonJournalActor.Input.Terminate
      sender() ! MasterCommand.Response.Accepted
  }

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit =
    event match {
      case event: OrderAdded ⇒
        val order = Order.fromOrderAdded(orderId, event)
        orderRegister.insert(order.id → OrderEntry(order))
        tryAttachOrderToAgent(order)

      case event: OrderEvent ⇒
        val orderEntry = orderRegister(orderId)
        event match {
          case event: OrderCoreEvent ⇒
            orderEntry.update(event)
          case OrderStdWritten(t, chunk) ⇒
            logger.info(s"$orderId $t: ${chunk.trim}")
        }
        event match {
          case _: OrderEvent.OrderReady.type if detachingSuspended ⇒
            stash()

          case OrderEvent.OrderReady ⇒
            detachOrderFromAgent(orderId)

          case OrderEvent.OrderMovedToMaster ⇒
            moveAhead(orderId)

          case _ ⇒
        }
    }

  private def moveAhead(orderId: OrderId): Unit = {
    val orderEntry = orderRegister(orderId)
    for (jobnet ← pathToJobnet.get(orderEntry.order.jobnetPath);
         node ← jobnet.idToNode.get(orderEntry.order.nodeId)) {
      node match {
        case _: Jobnet.JobNode ⇒
          tryAttachOrderToAgent(orderEntry.order.castState[Order.Idle])
        case _: Jobnet.EndNode ⇒
          persistAsync(KeyedEvent(OrderEvent.OrderFinished)(orderId)) { stamped ⇒
            logger.info(stamped.toString)
          }
      }
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.Idle]): Unit =
    for (jobnet ← pathToJobnet.get(order.nodeKey.jobnetPath);
         agentPath ← jobnet.agentPathOption(order.nodeKey.nodeId);
         agentEntry ← agentRegister.get(agentPath))
    {
      agentEntry.actor ! AgentDriver.Input.AttachOrder(order, jobnet.reduceForAgent(agentPath))
    }

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    for (agentPath ← orderRegister(orderId).order.agentPathOption)
      agentRegister(agentPath).actor ! AgentDriver.Input.DetachOrder(orderId)
}

object MasterOrderKeeper {
  private val SnapshotJsonFormat = TypedJsonFormat[Any](
    Subtype[OrderScheduleEndedAt],
    Subtype[Order[Order.State]],
    Subtype[AgentEventId])
  //Subtype[Jobnet])

  private[order] val MyJournalMeta = new JsonJournalMeta(SnapshotJsonFormat, MasterKeyedEventJsonFormat) with GzipCompression

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

  private class AgentRegister extends ActorRegister[AgentPath, AgentEntry](_.actor) {
    override def insert(kv: (AgentPath, AgentEntry)) = super.insert(kv)
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

    def update(event: OrderCoreEvent): Unit =
      order = order.update(event)
  }
}
