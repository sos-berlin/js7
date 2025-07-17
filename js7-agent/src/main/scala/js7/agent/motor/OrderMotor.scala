package js7.agent.motor

import cats.effect.kernel.Resource
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Ref, ResourceIO}
import cats.syntax.traverse.*
import fs2.Chunk
import js7.agent.command.AgentCommandToEventCalc
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachOrder, DetachOrder, MarkOrder, ReleaseEvents}
import js7.agent.motor.OrderMotor.*
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, joinStd, right, startAndForget}
import js7.base.catsutils.CatsExtensions.flatMapSome
import js7.base.crypt.Signed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.problem.Checked.{Ops, RichCheckedF}
import js7.base.service.Service
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.command.CancellationMode
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventCalc, KeyedEvent, TimeCtx}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, ItemDetached, SignedItemAttachedToMe}
import js7.data.item.InventoryItem
import js7.data.order.OrderEvent.{OrderDetachable, OrderDetached, OrderForked, OrderKillingMarked, OrderProcessed}
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentBundle, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPathControl}
import js7.journal.{FileJournal, Persisted}
import js7.subagent.director.SubagentKeeper

private final class OrderMotor private(
  jobMotor: JobMotor,
  orderQueue: Queue[IO, Option[Seq[Order[Order.State]]]],
  agentPath: AgentPath,
  subagentKeeper: SubagentKeeper[AgentState],
  journal: FileJournal[AgentState],
  agentConf: AgentConfiguration)
  (using
    clock: AlarmClock,
    dispatcher: Dispatcher[IO])
extends Service.StoppableByRequest:

  private val agentCommandToEventCalc = AgentCommandToEventCalc(agentConf)
  private val orderToEntry = AsyncMap[OrderId, OrderEntry]

  jobMotor.wireOnOrderProcessed(onOrderProcessed)

  protected def start =
    startService:
      (untilStopRequested *> orderQueue.offer(None))
        .background.surround:
          processQueue
      *> orderToEntry.toMap.values.foldMap(_.stop)

  override def stop: IO[Unit] =
    super.stop

  def recoverWorkflows(agentState: AgentState): IO[Unit] =
    agentState.idToWorkflow.values.foldMap:
      jobMotor.attachWorkflow

  def recoverOrders(agentState: AgentState): IO[Unit] =
    logger.debugIO:
      val orders = agentState.idToOrder.values.view
      val processingOrders = orders.flatMap(_.ifState[Order.Processing]).toVector
      val otherOrders = orders.filterNot(_.isState[Order.Processing]).toVector
      // Continue already processing Orders
      jobMotor.recoverProcessingOrders(processingOrders).flatMap: checkedFibers =>
        checkedFibers.foldMap: (orderId, checkedFiber) =>
          checkedFiber.traverse: fiber =>
            fiber.joinStd.flatMap: (_: OrderProcessed) =>
              enqueue(orderId)
            .handleErrorWith: t =>
              IO(logger.error(s"recoverOrderProcessing($orderId): ${t.toStringWithCauses}", t))
            .startAndForget
          .handleProblemWith: problem =>
            IO(logger.error(s"recoverOrderProcessing($orderId): $problem"))
      .productR:
        enqueue(otherOrders)

  def executeOrderCommand(cmd: AgentCommand.IsOrderCommand): IO[Checked[AgentCommand.Response]] =
    cmd match
      case _: AttachOrder | _: DetachOrder | _: MarkOrder | _: ReleaseEvents =>
        persist:
          agentCommandToEventCalc.commandToEventCalc(cmd)
        .rightAs(AgentCommand.Response.Accepted)

  def onItemEventPersisted(persisted: Persisted[AgentState, Event]): IO[Checked[Unit]] =
    persisted.keyedEvents.foldMap:
      case KeyedEvent(NoKey, ItemDetached(WorkflowId.as(workflowId), _)) =>
        jobMotor.detachWorkflow(workflowId)
          .map(Right(_))
      case KeyedEvent(NoKey, ItemAttachedToMe(item)) =>
        proceedWithItem(item)
      case KeyedEvent(NoKey, SignedItemAttachedToMe(Signed(workflow: Workflow, _))) =>
        jobMotor.attachWorkflow(persisted.aggregate.idToWorkflow(workflow.id))
          .map(Right(_))
      case _ =>
        IO.right(())

  def proceedWithItem(item: InventoryItem): IO[Checked[Unit]] =
    item match
      case agentRef: AgentRef =>
        jobMotor.tryStartProcessingAllJobs.map(Right(_))

      case subagentItem: SubagentItem =>
        journal.aggregate.flatMap:
          _.idToSubagentItemState.get(subagentItem.id)
            .fold(IO.pure(Checked.unit)): subagentItemState =>
              subagentKeeper.proceedWithSubagent(subagentItemState)
                .catchIntoChecked
        .flatMapT: _ =>
          jobMotor.tryStartProcessingAllJobs.map(Right(_))

      case subagentBundle: SubagentBundle =>
        subagentKeeper.addOrReplaceSubagentBundle(subagentBundle)
          .flatMapT: _ =>
            jobMotor.tryStartProcessingAllJobs.map(Right(_))

      case workflowPathControl: WorkflowPathControl =>
        if !workflowPathControl.suspended then
          enqueue:
            agentState().orders.view // Slow !!!
              .filter(_.workflowPath == workflowPathControl.workflowPath)
              .toVector
          .map(Right(_))
        else
          IO.right(())

      case _ => IO.right(())

  private def onOrderProcessed(orderId: OrderId): IO[Unit] =
    IO.unlessA(isStopping):
      enqueue(orderId)

  private def persist[E <: Event](toEvents: AgentState => Checked[IterableOnce[KeyedEvent[E]]])
  : IO[Checked[Persisted[AgentState, E]]] =
    persist(EventCalc.checked[AgentState, E, TimeCtx](toEvents(_)))

  private def persist[E <: Event](eventCalc: EventCalc[AgentState, E, TimeCtx])
  : IO[Checked[Persisted[AgentState, E]]] =
    journal.persist(eventCalc)
      .ifPersisted(onPersisted)

  private def onPersisted(persisted: Persisted[AgentState, Event]): IO[Unit] =
    persisted.keyedEvents.toVector.flatTraverse:
      case KeyedEvent(orderId: OrderId, event: OrderForked) =>
        IO.pure(orderId +: event.children.map(_.orderId))

      case KeyedEvent(orderId: OrderId, OrderKillingMarked(Some(kill))) =>
        persisted.aggregate.idToOrder.get(orderId).foldMap: order =>
          jobMotor.maybeKillOrder(order, kill)
        .as(Vector(orderId))

      case KeyedEvent(orderId: OrderId, OrderDetachable | OrderDetached) =>
        jobMotor.onOrderDetached(orderId, persisted.originalAggregate) *>
          orderToEntry.remove(orderId).flatMapSome:
            _.stop
          .as(Vector(orderId))

      case KeyedEvent(orderId: OrderId, _) =>
        IO.pure(Vector(orderId))

      case _ =>
        IO.pure(Vector.empty)
    .flatMap: touchedOrderIds =>
      enqueue:
        touchedOrderIds.distinct.flatMap:
          persisted.aggregate.idToOrder.get

  private def enqueue(orderId: OrderId): IO[Unit] =
    withCurrentOrder(orderId): order =>
      enqueue(Vector(order))

  private def enqueue(orders: Seq[Order[Order.State]]): IO[Unit] =
    IO.whenA(orders.nonEmpty):
      orderQueue.offer(Some(orders))

  private def processQueue: IO[Unit] =
    // TODO Beliebige Aufträge müssen aus orderQueue löschbar sein
    fs2.Stream.fromQueueNoneTerminated(orderQueue)
      .mapChunks: chunk =>
        Chunk(chunk.toVector.flatten
          // Keep last duplicates only
          .reverse.distinctBy(_.id).reverse)
      .evalMap:
        continueOrders
      .compile.drain

  private def continueOrders(orders: Vector[Order[Order.State]]): IO[Unit] =
    clock.lockIO: now =>
      val (immediateOrderIds, delayOrderIds) =
        orders.map: order =>
          order.id -> order.maybeDelayedUntil.getOrElse(Timestamp.Epoch)
        .partition(_._2 <= now)
      delayOrderIds.foldMap: (orderId, delayUntil) =>
        orderToEntry.getOrElseUpdate(orderId, IO.pure(OrderEntry(orderId))).flatMap: entry =>
          entry.schedule(delayUntil):
            enqueue(orderId) // Try again after delay
      .as(immediateOrderIds.map(_._1))
    .flatMap: (orderIds: Seq[OrderId] /*IntelliJ*/) =>
      persist: agentState =>
        Right:
          orderIds.flatMap: orderId =>
            if !agentState.idToOrder.contains(orderId) then
              Nil
            else
              OrderEventSource(agentState)(using InstructionExecutorService(clock))
                .nextEvents(orderId)
      .map(_.orThrow) // TODO throws
      .flatMap: persisted =>
        orderIds.foldMap: orderId =>
          persisted.aggregate.idToOrder.get(orderId).foldMap: order =>
            IO.whenA(persisted.aggregate.isOrderProcessable(orderId)):
              jobMotor.onOrderIsProcessable(order)

  private def withCurrentOrder[A](orderId: OrderId)(body: Order[Order.State] => IO[Unit]): IO[Unit] =
    journal.aggregate.flatMap: agentState =>
      agentState.idToOrder.get(orderId).foldMap:
        body

  private def agentState(): AgentState =
    journal.unsafeAggregate()

  override def toString = "OrderMotor"


object OrderMotor:
  private val logger = Logger[this.type]

  def service(
    agentPath: AgentPath,
    subagentKeeper: SubagentKeeper[AgentState],
    journal: FileJournal[AgentState],
    agentConf: AgentConfiguration)
    (using
      clock: AlarmClock,
      dispatcher: Dispatcher[IO])
  : ResourceIO[OrderMotor] =
    for
      jobMotor <- JobMotor.service(agentPath, subagentKeeper, journal.aggregate, agentConf)
      orderQueue <- Resource.eval(Queue.unbounded[IO, Option[Seq[Order[Order.State]]]])
      orderMotor <- Service.resource:
        new OrderMotor(jobMotor, orderQueue, agentPath, subagentKeeper, journal, agentConf)
    yield
      orderMotor


  private final class OrderEntry(orderId: OrderId):
    private val cancelScheduleRef = Ref.unsafe[IO, IO[Unit]](IO.unit)

    def stop: IO[Unit] =
      cancelScheduleRef.getAndSet(IO.unit).flatten

    def schedule(timestamp: Timestamp)(io: IO[Unit])
      (using clock: AlarmClock, dispatcher: Dispatcher[IO])
    : IO[Unit] =
      clock.scheduleIOAt(timestamp, label = orderId.toString):
        io
      .flatMap: cancel =>
        cancelScheduleRef.getAndSet(cancel)
          .flatten/*cancel previous schedule*/
