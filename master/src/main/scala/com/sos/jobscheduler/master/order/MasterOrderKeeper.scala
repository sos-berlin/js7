package com.sos.jobscheduler.master.order

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Stash, Status, Terminated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.ops.RichChecked
import com.sos.jobscheduler.base.utils.Collections.implicits.{InsertableMutableMap, RichTraversable, RichTraversableOnce}
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
import com.sos.jobscheduler.core.filebased.FileBaseds
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.FileBased
import com.sos.jobscheduler.data.filebased.FileBasedEvent.FileBasedAdded
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
import scala.collection.immutable.{Iterable, Seq}
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

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

  private var pathToNamedWorkflow = Map.empty[WorkflowPath, Workflow.Named]
  private val agentRegister = new AgentRegister
  private var scheduledOrderGenerators = Vector.empty[ScheduledOrderGenerator]
  private val orderRegister = mutable.Map[OrderId, OrderEntry]()
  private val idToOrder = orderRegister mapPartialFunction (_.order)
  private var orderProcessor = new OrderProcessor(PartialFunction.empty, PartialFunction.empty)
  private val orderScheduleGenerator = context.actorOf(
    Props { new OrderScheduleGenerator(journalActor = journalActor, masterOrderKeeper = self, masterConfiguration)},
    "OrderScheduleGenerator")

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    recover()
  }

  override def postStop() = {
    logger.debug("Stopped")
    super.postStop()
  }

  protected def snapshots = Future.successful(
    pathToNamedWorkflow.values.toImmutableIterable ++
    agentRegister.values.map(_.agent) ++
    agentRegister.values.map(entry ⇒ AgentEventId(entry.agentPath, entry.lastAgentEventId)) ++
    orderRegister.values.map(_ .order))

  private def recover() = {
    val recoverer = new MasterJournalRecoverer(journalFile = journalFile, orderScheduleGenerator = orderScheduleGenerator)
    recoverer.recoverAll()
    if (!recoverer.hasJournal) {
      readConfiguration().onProblem(o ⇒ logger.error(o))
    } else {
      readScheduledOrderGeneratorConfiguration().onProblem(o ⇒ logger.error(o))
      changeFileBaseds(recoverer.fileBaseds)
      for ((agentPath, eventId) ← recoverer.agentToEventId) {
        agentRegister(agentPath).lastAgentEventId = eventId
      }
      for (order ← recoverer.orders) {
        orderRegister.insert(order.id → OrderEntry(order))
      }
      updateOrderProcessor()
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
      val sender = this.sender()
      executeMasterCommand(command) onComplete {
        case Success(response) ⇒ sender ! response
        case Failure(t) ⇒ sender ! Status.Failure(t)
      }

    case Command.AddOrderSchedule(orders) ⇒
      for (order ← orders) {
        addOrder(order) onComplete {
          case Success(_) ⇒
          case Failure(t) ⇒ logger.error(t.toString, t)
        }
      }
      deferAsync {
        sender() ! Done
      }

    case Command.GetWorkflow(path) ⇒
      sender() ! pathToNamedWorkflow.get(path)

    case Command.GetWorkflows ⇒
      sender() ! eventIdGenerator.stamp(pathToNamedWorkflow.values.toVector: Vector[Workflow.Named])

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

    case Terminated(`journalActor`) ⇒
      logger.error("JournalActor terminated")
      context.stop(self)
  }

  private def executeMasterCommand(command: MasterCommand): Future[MasterCommand.Response] =
    command match {
      case MasterCommand.ReadConfigurationDirectory ⇒
        readConfiguration().map(_ ⇒ MasterCommand.Response.Accepted).toFuture

      case MasterCommand.ScheduleOrdersEvery(every) ⇒
        orderScheduleGenerator ! OrderScheduleGenerator.Input.ScheduleEvery(every.toJavaDuration)
        Future.successful(MasterCommand.Response.Accepted)

      case MasterCommand.AddOrderIfNew(order) ⇒
        addOrder(order) map (_ ⇒ MasterCommand.Response.Accepted)

      case MasterCommand.Terminate ⇒
        logger.info("Terminating by command")
        inhibitJournaling()
        orderScheduleGenerator ! PoisonPill
        journalActor ! JournalActor.Input.Terminate
        context.become(terminating)
        Future.successful(MasterCommand.Response.Accepted)
    }

  private def readConfiguration(): Checked[Unit] = {
    val existingFileBased = pathToNamedWorkflow.values.toImmutableIterable ++ agentRegister.values.map(_.agent) ++ scheduledOrderGenerators
    val readers = Set(WorkflowReader, AgentReader, new ScheduledOrderGeneratorReader(masterConfiguration.timeZone))
    for (events ← FileBaseds.readDirectory(masterConfiguration.liveDirectory, readers, existingFileBased)) yield {
      changeFileBaseds(events collect { case FileBasedAdded(o) ⇒ o })
      updateOrderProcessor()
    }
  }

  private def changeFileBaseds(fileBaseds: Iterable[FileBased]): Unit = {
    pathToNamedWorkflow ++= fileBaseds collect { case o: Workflow.Named ⇒ o } toKeyedMap (_.path)
    fileBaseds collect { case o: Agent ⇒ o } foreach registerAgent
    handleScheduledOrderGeneratorConfiguration(fileBaseds collect { case o: ScheduledOrderGenerator ⇒ o })
  }

  private def updateOrderProcessor(): Unit = {
    orderProcessor = new OrderProcessor(pathToNamedWorkflow mapPartialFunction (_.workflow), idToOrder)
  }

  /** Separate handling for developer-only ScheduledOrderGenerator, which are not journaled and read at every restart. */
  private def readScheduledOrderGeneratorConfiguration(): Checked[Completed] = {
    val reader = new ScheduledOrderGeneratorReader(masterConfiguration.timeZone)
    for (events ← FileBaseds.readDirectory(masterConfiguration.liveDirectory, reader :: Nil, scheduledOrderGenerators, ignoreAliens = true)) yield {
      handleScheduledOrderGeneratorConfiguration(events collect { case FileBasedAdded(o: ScheduledOrderGenerator) ⇒ o })
      Completed
    }
  }

  private def handleScheduledOrderGeneratorConfiguration(fileBaseds: Iterable[ScheduledOrderGenerator]): Unit = {
    scheduledOrderGenerators ++= fileBaseds
    orderScheduleGenerator ! OrderScheduleGenerator.Input.Change(scheduledOrderGenerators)
  }

  private def registerAgent(agent: Agent): Unit = {
    val actor = context.actorOf(
      Props { new AgentDriver(agent.path, agent.uri, masterConfiguration.config) },
      name = encodeAsActorName("Agent-" + agent.path.withoutStartingSlash))
    agentRegister.insert(agent.path → AgentEntry(agent, actor))
  }

  private def addOrder(order: Order[Order.Idle]): Future[Completed] =
    orderRegister.get(order.id) match {
      case None if pathToNamedWorkflow.isDefinedAt(order.workflowPath) ⇒
        persistAsync(KeyedEvent(OrderAdded(order.workflowPath, order.state, order.payload))(order.id)) { stamped ⇒
          handleOrderEvent(stamped)
          Completed
        }

      case None if !pathToNamedWorkflow.isDefinedAt(order.workflowPath) ⇒
        Future.failed(new NoSuchElementException(s"Unknown Workflow '${order.workflowPath.string}'"))

      case Some(_) ⇒
        logger.debug(s"Discarding duplicate AddOrderIfNew: ${order.id}")
        Future.successful(Completed)
    }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): Unit =
    handleOrderEvent(stamped.value.key, stamped.value.event)

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit = {
    logNotableEvent(orderId, event)
    event match {
      case event: OrderAdded ⇒
        registerOrderAndProceed(Order.fromOrderAdded(orderId, event))

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
                  registerOrderAndProceed(childOrder)

                case FollowUp.AddOffered(offeredOrder) ⇒
                  registerOrderAndProceed(offeredOrder)

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

  private def registerOrderAndProceed(order: Order[Order.State]): Unit = {
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

  private def terminating: Receive = {
    case _: MasterCommand ⇒
      sender() ! Status.Failure(new RuntimeException(s"Master is terminating"))

    case Terminated(`journalActor`) ⇒
      logger.error("Stop")
      context.stop(self)
  }

  override def toString = "MasterOrderKeeper"
}

object MasterOrderKeeper {
  private val SnapshotJsonCodec = TypedJsonCodec[Any](
    Subtype[Workflow.Named],
    Subtype[Agent],
    Subtype[AgentEventId],  // TODO case class AgentState(eventId: EventId)
    Subtype[OrderScheduleEndedAt],
    Subtype[Order[Order.State]])

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

  private class AgentRegister extends ActorRegister[AgentPath, AgentEntry](_.actor) {
    override def insert(kv: (AgentPath, AgentEntry)) = super.insert(kv)
  }

  private case class AgentEntry(
    agent: Agent,
    actor: ActorRef,
    var lastAgentEventId: EventId = EventId.BeforeFirst)
  {
    def agentPath = agent.path
  }

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
