package com.sos.jobscheduler.master.order

import akka.Done
import akka.actor.{ActorRef, Props, Stash, Status, Terminated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.ops.RichChecked
import com.sos.jobscheduler.base.utils.Collections.implicits.{InsertableMutableMap, RichTraversable}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.JournalRecoverer.startJournalAndFinishRecovery
import com.sos.jobscheduler.core.event.journal.{JournalActor, JournalMeta, JournalRecoverer, KeyedEventJournalingActor, RecoveredJournalingActors}
import com.sos.jobscheduler.core.filebased.FileBasedReader.readDirectoryTreeFlattenProblems
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAwaiting, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowPath, WorkflowPosition}
import com.sos.jobscheduler.master.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.order.MasterOrderKeeper._
import com.sos.jobscheduler.master.order.agent.{Agent, AgentDriver}
import com.sos.jobscheduler.master.workflow.WorkflowReader
import com.sos.jobscheduler.master.{AgentEventId, AgentEventIdEvent}
import java.time.Duration
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class MasterOrderKeeper(
  masterConfiguration: MasterConfiguration)
  (implicit
    timerService: TimerService,
    eventIdGenerator: EventIdGenerator,
    eventCollector: EventCollector,
    keyedEventBus: StampedKeyedEventBus)
extends KeyedEventJournalingActor[Event]
with Stash {

  override val supervisorStrategy = SupervisorStrategies.escalate

  import context.{become, dispatcher}
  intelliJuseImport(dispatcher)

  private val journalFile = masterConfiguration.stateDirectory / "journal"
  protected val journalActor = context.watch(context.actorOf(
    JournalActor.props(
      journalMeta(compressWithGzip = masterConfiguration.config.getBoolean("jobscheduler.master.journal.gzip")),
      journalFile, syncOnCommit = masterConfiguration.journalSyncOnCommit, eventIdGenerator, keyedEventBus),
    "Journal"))

  private val fileBaseds = readDirectoryTreeFlattenProblems(
      readers = Set(WorkflowReader, AgentReader, new ScheduledOrderGeneratorReader(masterConfiguration.timeZone)),
      directory = masterConfiguration.liveDirectory)
    .force
  private val pathToNamedWorkflow: Map[WorkflowPath, Workflow.Named] =
    fileBaseds collect { case o: Workflow.Named ⇒ o } toKeyedMap (_.path)
  private val agentRegister = new AgentRegister
  for (agent ← fileBaseds collect { case o: Agent ⇒ o }) {
    val actor = context.actorOf(
      Props { new AgentDriver(agent.path, agent.uri, masterConfiguration.config) },
      name = encodeAsActorName("Agent-" + agent.path.withoutStartingSlash))
    agentRegister.insert(agent.path → AgentEntry(agent.path, actor))
  }

  private val orderRegister = mutable.Map[OrderId, OrderEntry]()
  private val idToOrder = orderRegister mapPartialFunction (_.order)
  private val orderProcessor = new OrderProcessor(pathToNamedWorkflow mapPartialFunction (_.workflow), idToOrder)
  private val scheduledOrderGeneratorKeeper = new ScheduledOrderGeneratorKeeper(
    masterConfiguration,
    fileBaseds collect { case o: ScheduledOrderGenerator ⇒ o })
  private val orderScheduleGenerator = context.actorOf(
    Props { new OrderScheduleGenerator(journalActor = journalActor, masterOrderKeeper = self, scheduledOrderGeneratorKeeper)},
    "OrderScheduleGenerator")

  private var terminating = false

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    //keyedEventBus.subscribe(self, classOf[OrderEvent])
    recover()
  }

  override def postStop() = {
    //keyedEventBus.unsubscribe(self)
    logger.debug("Stopped")
    super.postStop()
  }

  protected def snapshots = Future.successful(
    (for (entry ← agentRegister.values) yield AgentEventId(entry.agentPath, entry.lastAgentEventId)) ++
      //??? pathToNamedWorkflow.values ++
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
    case JournalRecoverer.Output.JournalIsReady ⇒
      for (agentEntry ← agentRegister.values) {
        agentEntry.actor ! AgentDriver.Input.Start(lastAgentEventId = agentEntry.lastAgentEventId)
      }
      orderRegister.valuesIterator foreach proceedWithOrder
      logger.info(s"${orderRegister.size} Orders recovered, ready")
      become(ready)
      unstashAll()

    case _ ⇒ stash()
  }

  private def ready: Receive = journaling orElse {
    case command: MasterCommand ⇒
      executeMasterCommand(command)

    case Command.AddOrderSchedule(orders) ⇒
      for (order ← orders) {
        val logMsg = s"Order scheduled for ${order.state.at}: ${order.id}"
        orderRegister.get(order.id) match {
          case Some(_) ⇒
            logger.info(s"$logMsg is duplicate and discarded")
          case None if !pathToNamedWorkflow.isDefinedAt(order.workflowPath) ⇒
            logger.error(s"$logMsg: Unknown '${order.workflowPath}'")
          case _ ⇒
            persistAsync(KeyedEvent(OrderAdded(order.workflowPath, order.state, order.payload))(order.id)) { stamped ⇒
              handleOrderEvent(stamped)
            }
        }
      }
      deferAsync {
        sender() ! Done
      }

    case Command.GetWorkflow(path) ⇒
      sender() ! pathToNamedWorkflow.get(path)

    case Command.GetWorkflows ⇒
      sender() ! eventIdGenerator.stamp((pathToNamedWorkflow.values).toVector: Vector[Workflow.Named])

    case Command.GetWorkflowCount ⇒
      sender() ! (pathToNamedWorkflow.size: Int)

    case Command.GetOrder(orderId) ⇒
      sender() ! (orderRegister.get(orderId) map { _.order })

    case Command.GetOrders ⇒
      sender() ! eventIdGenerator.stamp((orderRegister.values map { _.order }).toVector: Vector[Order[Order.State]])

    case Command.GetOrderCount ⇒
      sender() ! (orderRegister.size: Int)

    case Command.Remove(orderId) ⇒
      orderRegister.get(orderId) match {
        case None ⇒ sender() ! Status.Failure(new NoSuchElementException(s"Unknown $orderId"))
        case Some(orderEntry) ⇒
          if (orderEntry.toBeRemoved)
            sender() ! Done
          else {
            orderEntry.toBeRemoved = true
            orderEntry.order.attachedTo match {
              case None ⇒
                //orderEntry.order = orderEntry.order.update(OrderRemoved)  // TODO Persist
                sender() ! Done

              case Some(Order.AttachedTo.AgentOrDetachable(agentPath)) ⇒
                sender() ! Status.Failure(new IllegalStateException(s"Order cannot be deleted because it is attached to Agent '$agentPath'"))
            }
          }
      }

    case AgentDriver.Output.EventsFromAgent(stampeds) ⇒
      val agentEntry = agentRegister(sender())
      import agentEntry.agentPath
      var lastAgentEventId = none[EventId]
      stampeds foreach {
        case Stamped(agentEventId, timestamp, KeyedEvent(orderId: OrderId, event: OrderEvent)) ⇒
          // OrderForked is (as all events) persisted and processed asynchronously,
          // so events for child orders will probably arrive before OrderForked has registered the child orderId.
          val ownEvent = event match {
            case _: OrderEvent.OrderAttached ⇒ OrderTransferredToAgent(agentPath)  // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
            case _ ⇒ event
          }
          persist(KeyedEvent(ownEvent)(orderId), Some(timestamp))(handleOrderEvent)
          lastAgentEventId = agentEventId.some
      }
      for (agentEventId ← lastAgentEventId) {
        persist(KeyedEvent(AgentEventIdEvent(agentEventId))(agentPath)) { e ⇒  // Sync
          agentEntry.lastAgentEventId = e.eventId
        }
      }

    case AgentDriver.Output.OrdersDetached(orderIds) ⇒
      val unknown = orderIds -- orderRegister.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Received OrdersDetached from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      for (orderId ← orderIds -- unknown) {
        persistAsync(KeyedEvent(OrderTransferredToMaster)(orderId))(handleOrderEvent)
      }

    case msg @ JournalActor.Output.SerializationFailure(throwable) ⇒
      logger.error(msg.toString, throwable)
      // Ignore this ???

    case Internal.Execute(callback) ⇒
      callback()

    case Terminated(`journalActor`) if terminating ⇒
      logger.info("Stop")
      context.stop(self)
  }

  def executeMasterCommand(command: MasterCommand): Unit = command match {
    case MasterCommand.ScheduleOrdersEvery(every) ⇒
      orderScheduleGenerator ! OrderScheduleGenerator.Input.ScheduleEvery(every.toJavaDuration)
      sender() ! MasterCommand.Response.Accepted

    case MasterCommand.AddOrderIfNew(order) ⇒
      orderRegister.get(order.id) match {
        case None if pathToNamedWorkflow.isDefinedAt(order.workflowPath) ⇒
          persistAsync(KeyedEvent(OrderAdded(order.workflowPath, order.state, order.payload))(order.id)) { stamped ⇒
            handleOrderEvent(stamped)
            sender() ! MasterCommand.Response.Accepted
          }
        case None if !pathToNamedWorkflow.isDefinedAt(order.workflowPath) ⇒
          sender() ! Status.Failure(new NoSuchElementException(s"Unknown Workflow '${order.workflowPath.string}'"))
        case Some(_) ⇒
          logger.debug(s"Discarding duplicate AddOrderIfNew: ${order.id}")
          sender() ! MasterCommand.Response.Accepted //Status.Failure(new IllegalStateException(s"Duplicate OrderId '${order.taskId}'"))
      }

    case MasterCommand.Terminate ⇒
      terminating = true
      journalActor ! JournalActor.Input.Terminate
      sender() ! MasterCommand.Response.Accepted
  }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): Unit =
    handleOrderEvent(stamped.value.key, stamped.value.event)

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit = {
    logNotableEvent(orderId, event)
    event match {
      case event: OrderAdded ⇒
        addOrderAndProceed(Order.fromOrderAdded(orderId, event))

      case _ ⇒
        orderRegister.get(orderId) match {
          case None ⇒
            logger.error(s"Unknown OrderId for event ${orderId <-: event}")

          case Some(orderEntry) ⇒
            val validatedFollowUps = orderProcessor.handleEvent(orderId <-: event)
            for (followUps ← validatedFollowUps onProblem (p ⇒ logger.error(p)))  {
              followUps foreach {
                case _: FollowUp.Processed if orderEntry.order.isAttachedToAgent ⇒

                case FollowUp.AddChild(childOrder) ⇒
                  addOrderAndProceed(childOrder)

                case FollowUp.AddOffered(offeredOrder) ⇒
                  addOrderAndProceed(offeredOrder)

                case FollowUp.Remove(removeOrderId) ⇒
                  orderRegister -= removeOrderId

                case unexpected ⇒
                  logger.error(s"Order '$orderId': Unexpected FollowUp $unexpected")
              }
            }
            orderEntry.update(event)
            proceedWithOrder(orderEntry)
        }
    }
  }

  private def addOrderAndProceed(order: Order[Order.State]): Unit = {
    val entry = OrderEntry(order)
    orderRegister.insert(order.id → entry)
    proceedWithOrder(entry)
  }

  private def proceedWithOrder(orderEntry: OrderEntry): Unit = {
    val order = orderEntry.order
    order.attachedTo match {
      case None ⇒
        proceedWithOrderOnMaster(orderEntry)

      case Some(_: Order.AttachedTo.Detachable) ⇒
        detachOrderFromAgent(order.id)

      case _ ⇒
    }
  }

  private def proceedWithOrderOnMaster(orderEntry: OrderEntry): Unit = {
    import orderEntry.order
    order.state match {
      case _: Order.Idle ⇒
        val idleOrder = order.castState[Order.Idle]
        instruction(order.workflowPosition) match {
          case _: Job ⇒ tryAttachOrderToAgent(idleOrder)
          case _ ⇒
        }

      case _: Order.Offered ⇒
        for (awaitingOrderId ← orderProcessor.offeredToAwaitingOrder(orderEntry.orderId);
             o ← orderRegister.get(awaitingOrderId);
             _ ← o.order.ifState[Order.Awaiting]/*must be*/) {
          proceedWithOrder(o)
        }

      case _ ⇒
    }

    for (keyedEvent ← orderProcessor.nextEvent(order.id).onProblem(p ⇒ logger.error(p)).flatten) {
      persistAsync(keyedEvent)(handleOrderEvent)
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.Idle]): Unit =
    for (namedWorkflow ← pathToNamedWorkflow.get(order.workflowPath);
         job ← namedWorkflow.workflow.jobOption(order.position);
         agentEntry ← agentRegister.get(job.agentPath))
    {
      agentEntry.actor ! AgentDriver.Input.AttachOrder(order, job.agentPath, namedWorkflow.workflow.reduceForAgent(job.agentPath))
    }

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    orderRegister(orderId).order.detachableFromAgent match {
      case Invalid(problem) ⇒ logger.error(s"detachOrderFromAgent '$orderId': not AttachedTo.Detachable: $problem")
      case Valid(agentPath) ⇒
        agentRegister(agentPath).actor ! AgentDriver.Input.DetachOrder(orderId)
    }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    pathToNamedWorkflow(workflowPosition.workflowPath).workflow.instruction(workflowPosition.position)

  override def toString = "MasterOrderKeeper"
}

object MasterOrderKeeper {
  private val SnapshotJsonCodec = TypedJsonCodec[Any](
    Subtype[OrderScheduleEndedAt],
    Subtype[Order[Order.State]],
    Subtype[AgentEventId])
  //Subtype[Workflow])

  private[order] def journalMeta(compressWithGzip: Boolean) =
    JournalMeta.gzipped(SnapshotJsonCodec, MasterKeyedEventJsonCodec, compressWithGzip = compressWithGzip)

  private val logger = Logger(getClass)

  sealed trait Command
  object Command {
    final case class AddOrderSchedule(orders: Seq[Order[Order.Scheduled]]) extends Command
    final case class GetWorkflow(path: WorkflowPath) extends Command
    case object GetWorkflows extends Command
    case object GetWorkflowCount extends Command
    final case class GetOrder(orderId: OrderId) extends Command
    final case object GetOrders extends Command
    final case object GetOrderCount extends Command
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

    def update(event: OrderEvent): Unit =
      event match {
        case _: OrderStdWritten ⇒
          logEvent(orderId, event)

        case event: OrderCoreEvent ⇒
          order = order.update(event)
      }
  }

  private def logNotableEvent(orderId: OrderId, event: OrderEvent): Unit =
    event match {
      case _ @  (_: OrderAdded | _: OrderTransferredToAgent | OrderTransferredToMaster | OrderFinished |
                 _: OrderForked | _: OrderJoined | _: OrderOffered | _: OrderAwaiting ) ⇒
        logEvent(orderId, event)
      case _ ⇒
    }

  private def logEvent(orderId: OrderId, event: OrderEvent): Unit =
    logger.info(s"🔶 ${KeyedEvent(event)(orderId)}")
}
