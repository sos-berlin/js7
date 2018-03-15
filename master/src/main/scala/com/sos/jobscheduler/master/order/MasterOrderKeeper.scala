package com.sos.jobscheduler.master.order

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Stash, Status, Terminated}
import akka.pattern.pipe
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.syntax.flatMap._
import cats.syntax.option._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
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
import com.sos.jobscheduler.core.filebased.{FileBaseds, Repo}
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent.FileBasedAdded
import com.sos.jobscheduler.data.filebased.{RepoEvent, VersionId}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAwaiting, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowPath, WorkflowPosition}
import com.sos.jobscheduler.master.KeyedEventJsonCodecs.{MasterFileBasedJsonCodec, MasterKeyedEventJsonCodec, MasterTypedPathCompanions, MasterTypedPathJsonCodec}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.master.order.MasterOrderKeeper._
import com.sos.jobscheduler.master.order.agent.{Agent, AgentDriver}
import com.sos.jobscheduler.master.{AgentEventId, AgentEventIdEvent}
import java.nio.file.Files
import scala.collection.immutable.Seq
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
extends Stash
with KeyedEventJournalingActor[Event] {

  override val supervisorStrategy = SupervisorStrategies.escalate

  import context.{become, dispatcher}
  intelliJuseImport(dispatcher)

  private val journalFile = masterConfiguration.stateDirectory / "journal"
  protected val journalActor = context.watch(context.actorOf(
    JournalActor.props(
      journalMeta(compressWithGzip = masterConfiguration.config.getBoolean("jobscheduler.master.journal.gzip")),
      journalFile, syncOnCommit = masterConfiguration.journalSyncOnCommit, eventIdGenerator, keyedEventBus),
    "Journal"))
  private var hasRecovered = false

  private val agentRegister = new AgentRegister
  private val orderRegister = mutable.Map[OrderId, OrderEntry]()
  private var scheduledOrderGenerators = Vector.empty[ScheduledOrderGenerator]
  private val idToOrder = orderRegister mapPartialFunction (_.order)
  private var orderProcessor = new OrderProcessor(PartialFunction.empty, idToOrder)
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
    fileBaseds.repo.eventsFor(MasterTypedPathCompanions) ++
    agentRegister.values.map(entry ⇒ AgentEventId(entry.agentPath, entry.lastAgentEventId)) ++
    orderRegister.values.map(_ .order))

  private def recover() = {
    val recoverer = new MasterJournalRecoverer(journalFile = journalFile, orderScheduleGenerator = orderScheduleGenerator)
    recoverer.recoverAll()
    if (recoverer.hasJournal) {
      hasRecovered = true
      fileBaseds.replaceRepo(recoverer.repo)
      for (agent ← fileBaseds.repo.currentFileBaseds collect { case o: Agent ⇒ o }) {
        registerAgent(agent)
      }
      for ((agentPath, eventId) ← recoverer.agentToEventId) {
        agentRegister(agentPath).lastAgentEventId = eventId
      }
      for (order ← recoverer.orders) {
        orderRegister.insert(order.id → OrderEntry(order))
      }
    }
    startJournalAndFinishRecovery(journalActor = journalActor,
      RecoveredJournalingActors(Map(OrderScheduleGenerator.Key → orderScheduleGenerator)))
  }

  def receive = journaling orElse {
    case JournalRecoverer.Output.JournalIsReady ⇒
      agentRegister.values foreach { _.start() }
      orderRegister.values.toVector/*copy*/ foreach proceedWithOrder
      logger.info(s"${orderRegister.size} Orders recovered")
      become(ready)
      unstashAll()
      if (!hasRecovered) {
        fileBaseds.readConfigurationAndPersistEvents(InitialVersion).force.unsafeRunSync()  // Persists events
      }
      readScheduledOrderGeneratorConfiguration().force.unsafeRunSync()
      defer {  // Publish after configuration events have been persisted and published
        logger.info("Ready")
        keyedEventBus.publish(eventIdGenerator.stamp(MasterEvent.MasterReady))
      }

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
        addOrderWithUncheckedId(order) onComplete {
          case Success(_: Boolean) ⇒
          case Failure(t) ⇒ logger.error(t.toString, t)
        }
      }
      deferAsync {
        sender() ! Done
      }

    case Command.GetWorkflow(path) ⇒
      sender() ! (fileBaseds.repo.currentTyped[Workflow].checked(path).toOption: Option[Workflow])

    case Command.GetWorkflows ⇒
      sender() ! eventIdGenerator.stamp(fileBaseds.pathToWorkflow.values.toVector: Vector[Workflow])

    case Command.GetWorkflowCount ⇒
      sender() ! (fileBaseds.pathToWorkflow.size: Int)

    case Command.AddOrder(order) ⇒
      addOrder(order) map Response.AddOrderAccepted.apply pipeTo sender()

    case Command.GetOrder(orderId) ⇒
      sender() ! (orderRegister.get(orderId) map { _.order })

    case Command.GetOrders ⇒
      sender() ! eventIdGenerator.stamp((orderRegister.values map { _.order }).toVector: Vector[Order[Order.State]])

    case Command.GetOrderCount ⇒
      sender() ! (orderRegister.size: Int)

    case Command.Remove(orderId) ⇒  // NOT TESTED
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
      //TODO journal transaction {
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
      case MasterCommand.ReadConfigurationDirectory(version) ⇒
        val checkedSideEffect = for {
          a ← fileBaseds.readConfigurationAndPersistEvents(version)
          b ← readScheduledOrderGeneratorConfiguration()
        } yield a >> b
        (for (sideEffect ← checkedSideEffect) yield {
          sideEffect.unsafeRunSync()
          MasterCommand.Response.Accepted
        }).toFuture

      case MasterCommand.ScheduleOrdersEvery(every) ⇒
        orderScheduleGenerator ! OrderScheduleGenerator.Input.ScheduleEvery(every.toJavaDuration)
        Future.successful(MasterCommand.Response.Accepted)

      case cmd: MasterCommand.AddOrderIfNew ⇒
        addOrder(cmd.toFreshOrder) map (_ ⇒ MasterCommand.Response.Accepted)

      case MasterCommand.EmergencyStop ⇒       // For completeness. RunningMaster has handled the command already
        Future.failed(new NotImplementedError)

      case MasterCommand.Terminate ⇒
        logger.info("Command Terminate")
        inhibitJournaling()
        orderScheduleGenerator ! PoisonPill
        journalActor ! JournalActor.Input.Terminate
        context.become(terminating)
        Future.successful(MasterCommand.Response.Accepted)
    }

  private object fileBaseds extends MasterRepoReader
  {
    protected def masterConfiguration = MasterOrderKeeper.this.masterConfiguration

    protected def onConfigurationRead(events: Seq[RepoEvent], repo: Repo, sideEffect: IO[Unit]) = {
      //TODO journal transaction {
      for (event ← events) {
        persist(KeyedEvent(event)) { stamped ⇒
          logNotableEvent(stamped)
        }
      }
      defer {
        replaceRepo(repo)
        sideEffect.unsafeRunSync()
      }
    }

    override def replaceRepo(repo: Repo) = {
      super.replaceRepo(repo)
      orderProcessor = new OrderProcessor(idToWorkflow, idToOrder)
    }

    protected def onAgentAdded(agent: Agent) =
      registerAgent(agent).start()
  }

  /** Separate handling for developer-only ScheduledOrderGenerator, which are not journaled and read at every restart. */
  private def readScheduledOrderGeneratorConfiguration(): Checked[IO[Unit]] = {
    val dir = masterConfiguration.orderGeneratorsDirectory
    if (!Files.exists(dir))
      Valid(IO.unit)
    else {
      val reader = new ScheduledOrderGeneratorReader(masterConfiguration.timeZone)
      for (events ← FileBaseds.readDirectory(reader :: Nil, dir, scheduledOrderGenerators, fileBaseds.repo.versionId)) yield
        IO {
          scheduledOrderGenerators ++= events collect { case FileBasedAdded(o: ScheduledOrderGenerator) ⇒ o }
          orderScheduleGenerator ! OrderScheduleGenerator.Input.Change(scheduledOrderGenerators)
        }
    }
  }

  private def registerAgent(agent: Agent): AgentEntry = {
    val actor = context.actorOf(
      Props { new AgentDriver(agent.path, agent.uri, masterConfiguration.config) },
      name = encodeAsActorName("Agent-" + agent.path.withoutStartingSlash))
    val entry = AgentEntry(agent, actor)
    agentRegister.insert(agent.path → entry)
    entry
  }

  private def addOrder(order: FreshOrder): Future[Boolean] =
    order.id.checkedNameSyntax.toFuture flatMap (_ ⇒ addOrderWithUncheckedId(order))

  private def addOrderWithUncheckedId(freshOrder: FreshOrder): Future[Boolean] = {
    val order = freshOrder.toOrder(fileBaseds.repo.versionId)
    orderRegister.get(order.id) match {
      case Some(_) ⇒
        logger.debug(s"Discarding duplicate AddOrderIfNew: ${order.id}")
        Future.successful(false)

      case None ⇒
        fileBaseds.idToWorkflow(order.workflowId) match {
          case Invalid(problem) ⇒ Future.failed(problem.throwable)
          case Valid(workflow) ⇒
            persistAsync(KeyedEvent(OrderAdded(workflow.id, order.state.scheduledAt, order.payload))(order.id)) { stamped ⇒
              handleOrderEvent(stamped)
              true
            }
        }
    }
  }

  private def handleOrderEvent(stamped: Stamped[KeyedEvent[OrderEvent]]): Unit = {
    logNotableEvent(stamped)
    handleOrderEvent(stamped.value.key, stamped.value.event)
  }

  private def handleOrderEvent(orderId: OrderId, event: OrderEvent): Unit = {
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
    for (workflow ← fileBaseds.idToWorkflow(order.workflowId).toOption;
         job ← workflow.jobOption(order.position);
         agentEntry ← agentRegister.get(job.agentPath))
    {
      agentEntry.actor ! AgentDriver.Input.AttachOrder(order, job.agentPath, workflow.reduceForAgent(job.agentPath))
    }

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    orderRegister(orderId).order.detachableFromAgent match {
      case Invalid(problem) ⇒ logger.error(s"detachOrderFromAgent '$orderId': not AttachedTo.Detachable: $problem")
      case Valid(agentPath) ⇒
        agentRegister(agentPath).actor ! AgentDriver.Input.DetachOrder(orderId)
    }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    fileBaseds.idToWorkflow(workflowPosition.workflowId).force.instruction(workflowPosition.position)

  private def terminating: Receive = {
    case _: MasterCommand ⇒
      sender() ! Status.Failure(new RuntimeException(s"Master is terminating"))

    case Terminated(`journalActor`) ⇒
      logger.info("Stop")
      context.stop(self)
  }

  override def toString = "MasterOrderKeeper"
}

object MasterOrderKeeper {
  @deprecated
  private val InitialVersion = VersionId("(initial)")  // ???

  private val SnapshotJsonCodec =
    TypedJsonCodec[Any](
      Subtype[RepoEvent],  // These events describe complete objects
      Subtype[Agent],
      Subtype[AgentEventId],  // TODO case class AgentState(eventId: EventId)
      Subtype[OrderScheduleEndedAt],
      Subtype[Order[Order.State]])

  private[order] def journalMeta(compressWithGzip: Boolean) =
    JournalMeta.gzipped(SnapshotJsonCodec, MasterKeyedEventJsonCodec, compressWithGzip = compressWithGzip)

  private val logger = Logger(getClass)

  sealed trait Command
  object Command {
    final case class AddOrderSchedule(orders: Seq[FreshOrder]) extends Command
    final case class GetWorkflow(path: WorkflowPath) extends Command
    case object GetWorkflows extends Command
    case object GetWorkflowCount extends Command
    final case class AddOrder(order: FreshOrder) extends Command
    final case class GetOrder(orderId: OrderId) extends Command
    final case object GetOrders extends Command
    final case object GetOrderCount extends Command
    @deprecated("NOT TESTED", "-")
    private[MasterOrderKeeper] final case class Remove(orderId: OrderId) extends Command
  }

  sealed trait Reponse
  object Response {
    final case class AddOrderAccepted(created: Boolean)
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

    def start()(implicit sender: ActorRef): Unit =
      actor ! AgentDriver.Input.Start(lastAgentEventId = lastAgentEventId)
  }

  private case class OrderEntry(
    var order: Order[Order.State],
    var toBeRemoved: Boolean = false)
  {
    def orderId = order.id

    def update(event: OrderEvent): Unit =
      event match {
        case _: OrderStdWritten ⇒
        case event: OrderCoreEvent ⇒ order = order.update(event)
      }
  }

  private def logNotableEvent(stamped: Stamped[AnyKeyedEvent]): Unit =
    stamped.value.event match {
      case _ @ (_: OrderAdded | _: OrderTransferredToAgent | OrderTransferredToMaster | OrderFinished |
                _: OrderForked | _: OrderJoined | _: OrderOffered | _: OrderAwaiting | _: OrderStdWritten |
                _: RepoEvent ) ⇒
        def string = if (stamped.value.key == NoKey) stamped.value.event.toString else stamped.value.toString
        logger.info(Logger.Event, s"${stamped.timestamp} 🔶 $string")
      case _ ⇒
    }
}
