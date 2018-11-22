package com.sos.jobscheduler.master

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Stash, Status, Terminated}
import akka.pattern.pipe
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.traverse._
import com.sos.jobscheduler.agent.data.event.AgentMasterEvent
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichPartialFunction, RichThrowable}
import com.sos.jobscheduler.common.akkautils.Akkas.encodeAsActorName
import com.sos.jobscheduler.common.akkautils.SupervisorStrategies
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.event.{EventIdClock, EventIdGenerator}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops._
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.{JournalMeta, RecoveredJournalingActors}
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.filebased.{FileBaseds, Repo}
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderProcessor
import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.agent.{Agent, AgentId, AgentPath}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.RepoEvent.FileBasedAdded
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath, VersionId}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAwaiting, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderOffered, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import com.sos.jobscheduler.master.MasterOrderKeeper._
import com.sos.jobscheduler.master.agent.{AgentDriver, AgentEventIdEvent}
import com.sos.jobscheduler.master.command.CommandMeta
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentReady
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.master.scheduledorder.{OrderScheduleGenerator, ScheduledOrderGenerator, ScheduledOrderGeneratorReader}
import java.nio.file.Files
import java.time.ZoneId
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class MasterOrderKeeper(
  masterConfiguration: MasterConfiguration,
  journalMeta: JournalMeta[Event],
  eventWatch: JournalEventWatch[Event],
  eventIdClock: EventIdClock)
  (implicit
    keyedEventBus: StampedKeyedEventBus,
    scheduler: Scheduler)
extends Stash
with MainJournalingActor[Event]
{
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val eventIdGenerator = new EventIdGenerator(eventIdClock)
  protected val journalActor = watch(actorOf(
    JournalActor.props(
      journalMeta,
      masterConfiguration.config,
      keyedEventBus, scheduler,
      eventIdGenerator = eventIdGenerator),
    "Journal"))
  private var hasRecovered = false
  private val repoReader = new MasterRepoReader(masterConfiguration.fileBasedDirectory)
  private var repo = Repo.empty
  private val agentRegister = new AgentRegister
  private val orderRegister = mutable.Map[OrderId, OrderEntry]()
  private var scheduledOrderGenerators = Vector.empty[ScheduledOrderGenerator]
  private val idToOrder = orderRegister mapPartialFunction (_.order)
  private var orderProcessor = new OrderProcessor(PartialFunction.empty, idToOrder)
  private val orderScheduleGenerator = actorOf(
    Props { new OrderScheduleGenerator(journalActor = journalActor, masterOrderKeeper = self, masterConfiguration)},
    "OrderScheduleGenerator")
  private val suppressOrderIdCheckFor = masterConfiguration.config.optionAs[String]("jobscheduler.TEST-ONLY.suppress-order-id-check-for")
  private var terminating = false
  private var terminateRespondedAt: Option[Timestamp] = None

  private object afterProceedEvents {
    private val events = mutable.Buffer[KeyedEvent[OrderEvent]]()

    def persistAndHandleLater(keyedEvent: KeyedEvent[OrderEvent]): Unit = {
      val first = events.isEmpty
      events += keyedEvent
      if (first) {
        self ! Internal.AfterProceedEventsAdded
      }
    }

    def persistThenHandleEvents(): Unit = {
      // Eliminate duplicate events like OrderJoined, which may be issued by parent and child orders when recovering
      persistMultiple(events.distinct)(
        _ foreach handleOrderEvent)
      events.clear()
    }
  }

  override def preStart() = {
    super.preStart()  // First let JournalingActor register itself
    recover()
  }

  override def postStop() = {
    super.postStop()
    for (t ← terminateRespondedAt) {
      val millis = (t + 500.millis - now).toMillis
      if (millis > 0) {
        logger.debug("Delaying to let HTTP server respond to Terminate command")
        blocking {
          Thread.sleep(millis)
        }
      }
    }
    logger.debug("Stopped")
  }

  protected def snapshots = Future.successful(
    MasterState(
      eventIdGenerator.lastUsedEventId,
      repo,
      orderRegister.mapValues(_.order).toMap,
      agentRegister.values.map(entry ⇒ entry.agentId → entry.lastAgentEventId).toMap,
      orderScheduleEndedAt = None)
    .toSnapshots)

  private def recover() = {
    val recoverer = new MasterJournalRecoverer(journalMeta)
    recoverer.recoverAll()
    for (masterState ← recoverer.masterState) {
      hasRecovered = true
      changeRepo(masterState.repo)
      for (agent ← repo.currentFileBaseds collect { case o: Agent ⇒ o }) {
        registerAgent(agent)
      }
      for ((agentId, eventId) ← masterState.agentToEventId) {
        agentRegister(agentId).lastAgentEventId = eventId
      }
      for (order ← masterState.idToOrder.values) {
        orderRegister.insert(order.id → OrderEntry(order))
      }
      for (at ← masterState.orderScheduleEndedAt) {
        orderScheduleGenerator ! OrderScheduleGenerator.Input.Recover(at)
      }
    }
    recoverer.startJournalAndFinishRecovery(
      journalActor = journalActor,
      RecoveredJournalingActors(Map(OrderScheduleGenerator.Key → orderScheduleGenerator)),
      Some(eventWatch))
  }

  def receive = {
    case JournalRecoverer.Output.JournalIsReady ⇒
      agentRegister.values foreach { _.start() }

      if (hasRecovered) {
        orderRegister.values.toVector/*copy*/ foreach proceedWithOrder  // Any ordering when continuing orders???
        afterProceedEvents.persistThenHandleEvents()  // Persist and handle before Internal.Ready
        logger.info(s"${orderRegister.size} Orders recovered")
      } else {
        readConfiguration(InitialVersion.some).orThrow.unsafeRunSync()  // Persists events
      }
      readScheduledOrderGeneratorConfiguration().orThrow.unsafeRunSync()
      persist(MasterEvent.MasterReady(masterConfiguration.masterId, ZoneId.systemDefault))(_ ⇒
        self ! Internal.Ready
      )

    case Internal.Ready ⇒
      logger.info("Ready")
      become("Ready")(ready)
      unstashAll()

    case _ ⇒ stash()
  }

  private def ready: Receive = {
    case Internal.AfterProceedEventsAdded ⇒
      afterProceedEvents.persistThenHandleEvents()

    case Command.Execute(command, meta) ⇒
      val sender = this.sender()
      if (terminating)
        sender ! Invalid(MasterIsTerminatingProblem)
      else
        executeMasterCommand(command, meta).runOnComplete {
          case Success(response) ⇒ sender ! response
          case Failure(t) ⇒ sender ! Status.Failure(t)
        }

    case Command.AddOrderSchedule(orders) if !terminating ⇒
      for (order ← orders) {
        addOrderWithUncheckedId(order) onComplete {
          case Failure(t) ⇒ logger.error(s"AddOrderSchedule: ${t.toStringWithCauses}", t)
          case Success(Invalid(problem)) ⇒ logger.error(problem withPrefix "AddOrderSchedule:")
          case Success(Valid(false)) ⇒ logger.warn(s"AddOrderSchedule: Discarded duplicate order ${order.id}")
          case Success(Valid(true)) ⇒
        }
      }
      deferAsync {
        sender() ! Done
      }

    case Command.GetRepo ⇒
      sender() ! eventIdGenerator.stampWithLast(repo)

    case Command.AddOrder(order) ⇒
      if (terminating)
        sender ! Status.Failure(MasterIsTerminatingProblem.throwable)
      else
        addOrder(order) map Response.ForAddOrder.apply pipeTo sender()

    case Command.GetOrder(orderId) ⇒
      sender() ! (orderRegister.get(orderId) map { _.order })

    case Command.GetOrders ⇒
      sender() ! eventIdGenerator.stampWithLast((orderRegister.values map { _.order }).toVector: Vector[Order[Order.State]])

    case Command.GetOrderCount ⇒
      sender() ! (orderRegister.size: Int)

    case AgentDriver.Output.EventsFromAgent(stampeds) ⇒
      val agentEntry = agentRegister(sender())
      import agentEntry.agentId
      var lastAgentEventId = none[EventId]
      var masterStamped: Seq[Timestamped[Event]] = stampeds.flatMap {
        case stamped @ Stamped(agentEventId, timestamp, keyedEvent) ⇒
          if (agentEventId <= lastAgentEventId.getOrElse(agentEntry.lastAgentEventId)) {
            logger.error(s"Agent ${agentEntry.agentId} has returned old (<= ${lastAgentEventId.getOrElse(agentEntry.lastAgentEventId)}) event: $stamped")
            None
          } else
            keyedEvent match {
              case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
                val ownEvent = event match {
                  case _: OrderEvent.OrderAttached ⇒ OrderTransferredToAgent(agentId) // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                  case _ ⇒ event
                }
                lastAgentEventId = agentEventId.some
                Some(Timestamped(orderId <-: ownEvent, Some(timestamp)))

              case KeyedEvent(_: NoKey, AgentMasterEvent.AgentReadyForMaster(timezone)) ⇒
                lastAgentEventId = agentEventId.some
                Some(Timestamped(agentEntry.agentId.path <-: AgentReady(timezone), Some(timestamp)))
            }
      }
      masterStamped ++= lastAgentEventId.map(agentEventId ⇒ Timestamped(agentId <-: AgentEventIdEvent(agentEventId)))

      persistTransactionTimestamped(masterStamped) {
        _ map (_.value) foreach {
          case KeyedEvent(orderId: OrderId, event: OrderEvent) ⇒
            handleOrderEvent(orderId, event)

          case KeyedEvent(_: AgentId, AgentEventIdEvent(agentEventId)) ⇒
            agentEntry.lastAgentEventId = agentEventId
            agentEntry.actor ! AgentDriver.Input.EventsAccepted(agentEventId)

          case _ ⇒
        }
      }

    case AgentDriver.Output.OrdersDetached(orderIds) ⇒
      val unknown = orderIds -- orderRegister.keySet
      if (unknown.nonEmpty) {
        logger.error(s"Response to AgentCommand.DetachOrder from Agent for unknown orders: "+ unknown.mkString(", "))
      }
      persistMultipleAsync(orderIds -- unknown map (_ <-: OrderTransferredToMaster))(
        _ foreach handleOrderEvent)

    case JournalActor.Output.SnapshotTaken ⇒
      if (terminating) {
        // TODO termination wie in AgentOrderKeeper, dabei AgentDriver ordentlich beenden
        orderScheduleGenerator ! PoisonPill
        if (agentRegister.nonEmpty)
          agentRegister.values foreach { _.actor ! AgentDriver.Input.Terminate }
        else
          journalActor ! JournalActor.Input.Terminate
      }

    case Terminated(a) if agentRegister contains a ⇒
      agentRegister -= a
      if (agentRegister.isEmpty) {
        journalActor ! JournalActor.Input.Terminate
      }

    case Terminated(`journalActor`) ⇒
      if (!terminating) logger.error("JournalActor terminated")
      logger.info("Stop")
      context.stop(self)
  }

  private def executeMasterCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[MasterCommand.Response]] =
    command match {
      case MasterCommand.KeepEvents(eventId) ⇒
        Task {
          eventWatch.keepEvents(eventId)
            .map (_ ⇒ MasterCommand.Response.Accepted)
        }

      case MasterCommand.ReadConfigurationDirectory(versionId) ⇒
        val checkedSideEffect = for {
          a ← readConfiguration(versionId)  // Persists events
          b ← readScheduledOrderGeneratorConfiguration()
        } yield a >> b
        Task.now(
          for (sideEffect ← checkedSideEffect) yield {
            sideEffect.unsafeRunSync()
            MasterCommand.Response.Accepted
          })

      case MasterCommand.ScheduleOrdersEvery(every) ⇒
        orderScheduleGenerator ! OrderScheduleGenerator.Input.ScheduleEvery(every)
        Task.now(Valid(MasterCommand.Response.Accepted))

      case MasterCommand.EmergencyStop ⇒       // For completeness. RunningMaster has handled the command already
        Task.now(Invalid(Problem("NOT IMPLEMENTED")))  // Never called

      case MasterCommand.Terminate ⇒
        logger.info("Command Terminate")
        journalActor ! JournalActor.Input.TakeSnapshot
        terminating = true
        terminateRespondedAt = Some(now)
        Task.now(Valid(MasterCommand.Response.Accepted))
    }

  private def readConfiguration(versionId: Option[VersionId]): Checked[IO[Unit]] = {
    def updateFileBaseds(diff: FileBaseds.Diff[TypedPath, FileBased]): Seq[Checked[IO[Unit]]] =
      updateAgents(diff.select[AgentPath, Agent])

    def updateAgents(diff: FileBaseds.Diff[AgentPath, Agent]): Seq[Checked[IO[Unit]]] =
      onlyAdditionPossible(diff) :+
        Valid(IO {
          for (agent ← diff.added) registerAgent(agent).start()
        })

    def onlyAdditionPossible[P <: TypedPath, A <: FileBased](diff: FileBaseds.Diff[P, A]): Seq[Invalid[Problem]] =
      diff.deleted.map(o ⇒ Invalid(Problem(s"Deletion of configuration files is not supported: $o"))) ++:
        diff.changed.map(o ⇒ Invalid(Problem(s"Change of configuration files is not supported: ${o.path}")))

    for {
      eventsAndRepo ← repoReader.readConfiguration(repo, versionId)
      (events, changedRepo) = eventsAndRepo
      checkedSideEffects = updateFileBaseds(FileBaseds.Diff.fromEvents(events))
      foldedSideEffects ← checkedSideEffects.toVector.sequence map (_.fold(IO.unit)(_ >> _))  // One problem invalidates all side effects
    } yield IO {
      persistTransaction(events map (e ⇒ KeyedEvent(e)))(_ foreach logNotableEvent)
      defer {
        changeRepo(changedRepo)
        foldedSideEffects.unsafeRunSync()
      }
    }
  }

  private def changeRepo(o: Repo): Unit = {
    repo = o
    orderProcessor = new OrderProcessor(repo.idTo[Workflow], idToOrder)
  }

  private def registerAgent(agent: Agent): AgentEntry = {
    val actor = watch(actorOf(
      AgentDriver.props(agent.id, agent.uri, masterConfiguration, journalActor = journalActor),
      encodeAsActorName("Agent-" + agent.id.toSimpleString.stripPrefix("/"))))
    val entry = AgentEntry(agent, actor)
    agentRegister.insert(agent.id → entry)
    entry
  }

  /** Separate handling for developer-only ScheduledOrderGenerator, which are not journaled and read at every restart. */
  private def readScheduledOrderGeneratorConfiguration(): Checked[IO[Unit]] = {
    val dir = masterConfiguration.orderGeneratorsDirectory
    if (!Files.exists(dir))
      Valid(IO.unit)
    else {
      val reader = new ScheduledOrderGeneratorReader(masterConfiguration.timeZone)
      for (events ← FileBaseds.readDirectory(reader :: Nil, dir, scheduledOrderGenerators, repo.versionId)) yield
        IO {
          scheduledOrderGenerators ++= events collect { case FileBasedAdded(o: ScheduledOrderGenerator) ⇒ o }
          orderScheduleGenerator ! OrderScheduleGenerator.Input.Change(scheduledOrderGenerators)
        }
    }
  }

  private def addOrder(order: FreshOrder): Future[Checked[Boolean]] =
    suppressOrderIdCheckFor match {
      case Some(order.id.string) ⇒  // Test only
        addOrderWithUncheckedId(order)

      case _ ⇒
        order.id.checkedNameSyntax match {
          case Invalid(problem) ⇒ Future.successful(Invalid(problem))
          case Valid(_) ⇒ addOrderWithUncheckedId(order)
        }
    }

  private def addOrderWithUncheckedId(freshOrder: FreshOrder): Future[Checked[Boolean]] = {
    val order = freshOrder.toOrder(repo.versionId)
    orderRegister.get(order.id) match {
      case Some(_) ⇒
        logger.debug(s"Discarding duplicate added Order: $freshOrder")
        Future.successful(Valid(false))

      case None ⇒
        repo.idTo[Workflow](order.workflowId) match {
          case Invalid(problem) ⇒ Future.successful(Invalid(problem))
          case Valid(workflow) ⇒
            persist/*Async?*/(order.id <-: OrderAdded(workflow.id, order.state.scheduledAt, order.payload)) { stamped ⇒
              handleOrderEvent(stamped)
              Valid(true)
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
            val checkedFollowUps = orderProcessor.handleEvent(orderId <-: event)
            for (followUps ← checkedFollowUps onProblem (p ⇒ logger.error(p)))  {
              followUps foreach {
                case _: FollowUp.Processed if orderEntry.order.isAttached ⇒

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
            if (orderRegister contains orderId) {  // orderEntry has not been deleted?
              proceedWithOrder(orderEntry)
            }
        }
    }
  }

  private def registerOrderAndProceed(order: Order[Order.State]): Unit = {
    val entry = OrderEntry(order)
    orderRegister.insert(order.id → entry)
    proceedWithOrder(entry)
  }

  private def proceedWithOrder(orderEntry: OrderEntry): Unit =
    if (!terminating) {
      val order = orderEntry.order
      order.attachedState match {
        case None |
             Some(_: Order.Attaching) ⇒ proceedWithOrderOnMaster(orderEntry)
        case Some(_: Order.Attached)  ⇒
        case Some(_: Order.Detaching) ⇒ detachOrderFromAgent(order.id)
      }
    }

  private def proceedWithOrderOnMaster(orderEntry: OrderEntry): Unit = {
    val order = orderEntry.order
    order.state match {
      case _: Order.FreshOrReady ⇒
        val freshOrReady = order.castState[Order.FreshOrReady]
        instruction(order.workflowPosition) match {
          case _: Execute ⇒ tryAttachOrderToAgent(freshOrReady)
          case _ ⇒
        }

      case _: Order.Offering ⇒
        for (awaitingOrderId ← orderProcessor.offeredToAwaitingOrder(orderEntry.orderId);
             awaitingOrder ← orderRegister.checked(awaitingOrderId).onProblem(p ⇒ logger.warn(p.toString));
             _ ← awaitingOrder.order.checkedState[Order.Awaiting].onProblem(p ⇒ logger.error(p.toString)))
        {
          proceedWithOrderOnMaster(awaitingOrder)
        }

      case _ ⇒
    }

    // When recovering, proceedWithOrderOnMaster may issue the same event multiple times, so OrderJoined for each parent and child order.
    // These events are collected and with actor message Internal.AfterProceedEventsAdded reduced to one.
    for (keyedEvent ← orderProcessor.nextEvent(order.id).onProblem(p ⇒ logger.error(p)).flatten) {
      afterProceedEvents.persistAndHandleLater(keyedEvent)
    }
  }

  private def tryAttachOrderToAgent(order: Order[Order.FreshOrReady]): Unit = {
    (for {
      workflow ← repo.idTo[Workflow](order.workflowId)
      job ← workflow.checkedWorkflowJob(order.position)
      agentId ← repo.pathToCurrentId(job.agentPath)
      agentEntry ← agentRegister.checked(agentId)
    } yield {
      if (order.isDetached)
        persist(order.id <-: OrderAttachable(agentId.path)) { stamped ⇒
          handleOrderEvent(stamped)
        }
      else
        agentEntry.actor ! AgentDriver.Input.AttachOrder(order, agentId, workflow.reduceForAgent(job.agentPath))
      ()
    }).onProblem(p ⇒ logger.error(p))
  }

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    orderRegister(orderId).order.detaching
      .onProblem(p ⇒ logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
      .foreach { agentId ⇒
        agentRegister(agentId).actor ! AgentDriver.Input.DetachOrder(orderId)
      }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    repo.idTo[Workflow](workflowPosition.workflowId).orThrow.instruction(workflowPosition.position)

  override def toString = "MasterOrderKeeper"
}

private[master] object MasterOrderKeeper {
  private val MasterIsTerminatingProblem = Problem.eager("Master is terminating")
  @deprecated("", "")
  private val InitialVersion = VersionId("(initial)")  // ???

  private val logger = Logger(getClass)

  sealed trait Command
  object Command {
    final case class Execute(command: MasterCommand, meta: CommandMeta)
    final case class AddOrderSchedule(orders: Seq[FreshOrder]) extends Command
    final case object GetRepo extends Command
    final case class AddOrder(order: FreshOrder) extends Command
    final case class GetOrder(orderId: OrderId) extends Command
    final case object GetOrders extends Command
    final case object GetOrderCount extends Command
  }

  sealed trait Reponse
  object Response {
    final case class ForAddOrder(created: Checked[Boolean])
  }

  private object Internal {
    case object Ready
    case object AfterProceedEventsAdded
  }

  private class AgentRegister extends ActorRegister[AgentId, AgentEntry](_.actor) {
    override def insert(kv: (AgentId, AgentEntry)) = super.insert(kv)
    override def -=(a: ActorRef) = super.-=(a)
  }

  private case class AgentEntry(
    agent: Agent,
    actor: ActorRef,
    var lastAgentEventId: EventId = EventId.BeforeFirst)
  {
    def agentId = agent.id
    @deprecated("", "")
    def agentPath = agent.path

    def start()(implicit sender: ActorRef): Unit =
      actor ! AgentDriver.Input.Start(lastAgentEventId = lastAgentEventId)
  }

  private case class OrderEntry(var order: Order[Order.State])
  {
    def orderId = order.id

    def update(event: OrderEvent): Unit =
      event match {
        case _: OrderStdWritten ⇒
        case event: OrderCoreEvent ⇒ order = order.update(event).orThrow  // 🔥 ProblemException
      }
  }

  private def logNotableEvent(stamped: Stamped[AnyKeyedEvent]): Unit =
    if (false)
      stamped.value.event match {
      case _ @ (_: OrderAdded | _: OrderTransferredToAgent | OrderTransferredToMaster | OrderFinished |
                _: OrderForked | _: OrderJoined | _: OrderOffered | _: OrderAwaiting | _: OrderStdWritten |
                _: RepoEvent ) ⇒
        def string = if (stamped.value.key == NoKey) stamped.value.event.toString else stamped.value.toString
        logger.debug(Logger.Event, s"${stamped.timestamp} $string")
      case _ ⇒
    }
}
