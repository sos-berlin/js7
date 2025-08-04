package js7.agent.motor

import cats.effect.kernel.Resource
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Ref, ResourceIO}
import cats.syntax.traverse.*
import js7.agent.command.AgentCommandToEventCalc
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachOrder, DetachOrder, MarkOrder, ReleaseEvents}
import js7.agent.motor.OrderMotor.*
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, joinStd, right, startAndForget}
import js7.base.catsutils.CatsExtensions.flatMapSome
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.Service
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.command.CancellationMode
import js7.data.event.{Event, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.InventoryItem
import js7.data.order.OrderEvent.{OrderDetachable, OrderDetached, OrderForked, OrderKillingMarked, OrderProcessed}
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentBundle, SubagentItem}
import js7.data.workflow.WorkflowPathControl
import js7.journal.{FileJournal, Persisted}
import js7.subagent.director.SubagentKeeper

private final class OrderMotor private(
  orderQueue: Queue[IO, Option[Seq[Order[Order.State]]]],
  agentPath: AgentPath,
  subagentKeeper: SubagentKeeper[AgentState],
  journal: FileJournal[AgentState],
  bean: Bean,
  agentConf: AgentConfiguration)
  (using
    clock: AlarmClock,
    dispatcher: Dispatcher[IO])
extends Service.StoppableByRequest:

  private val agentCommandToEventCalc = AgentCommandToEventCalc(agentConf)
  private val orderToEntry = AsyncMap[OrderId, OrderEntry]
  private[motor] val jobMotorKeeper =
    new JobMotorKeeper(agentPath, this, subagentKeeper, journal.aggregate, agentConf)

  protected def start =
    startService:
      (untilStopRequested *> orderQueue.offer(None))
        .background.surround:
          runOrderPipeline
      *> orderToEntry.toMap.values.foldMap(_.stop)
      *> jobMotorKeeper.stop

  override def stop: IO[Unit] =
    super.stop

  def recoverWorkflows(agentState: AgentState): IO[Unit] =
    agentState.idToWorkflow.values.foldMap:
      jobMotorKeeper.attachWorkflow

  def recoverOrders(agentState: AgentState): IO[Unit] =
    logger.debugIO:
      val orders = agentState.idToOrder.values.view
      val processingOrders = orders.flatMap(_.ifState[Order.Processing]).toVector
      val otherOrders = orders.filterNot(_.isState[Order.Processing]).toVector
      // Continue already processing Orders
      jobMotorKeeper.recoverProcessingOrders(processingOrders, agentState).flatMap: checkedFibers =>
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
        journal.persist:
          agentCommandToEventCalc.commandToEventCalc(cmd)
        .ifPersisted:
          onPersisted
        .rightAs(AgentCommand.Response.Accepted)

  def proceedWithItem(item: InventoryItem): IO[Checked[Unit]] =
    item match
      case agentRef: AgentRef =>
        jobMotorKeeper.triggerAllJobs(reason = item.key).map(Right(_))

      case subagentItem: SubagentItem =>
        journal.aggregate.flatMap:
          _.idToSubagentItemState.get(subagentItem.id)
            .fold(IO.pure(Checked.unit)): subagentItemState =>
              subagentKeeper.proceedWithSubagent(subagentItemState)
                .catchIntoChecked
        .flatMapT: _ =>
          jobMotorKeeper.triggerAllJobs(reason = item.key).map(Right(_))

      case subagentBundle: SubagentBundle =>
        subagentKeeper.addOrReplaceSubagentBundle(subagentBundle)
          .flatMapT: _ =>
            jobMotorKeeper.triggerAllJobs(reason = item.key).map(Right(_))

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

  def trigger(orderId: OrderId): IO[Unit] =
    enqueue(orderId)

  private def onPersisted(persisted: Persisted[AgentState, Event]): IO[Unit] =
    persisted.keyedEvents.toVector.flatTraverse:
      case KeyedEvent(orderId: OrderId, event) =>
        event.match
          case event: OrderForked =>
            IO.pure(event.children.map(_.orderId))

          case OrderKillingMarked(Some(kill)) =>
            persisted.aggregate.idToOrder.get(orderId).foldMap: order =>
              jobMotorKeeper.maybeKillOrder(order, kill)
            .as(Vector.empty)

          case OrderDetachable | OrderDetached =>
            jobMotorKeeper.onOrderDetached(orderId, persisted.originalAggregate) *>
              orderToEntry.remove(orderId).flatMapSome:
                _.stop
              .as(Vector.empty)

          case _ =>
            IO.pure(Vector.empty)
        .map: touchedOrderIds =>
          touchedOrderIds :+ orderId
      case _ =>
        IO.pure(Vector.empty)
    .flatMap: (touchedOrderIds: Seq[OrderId]/*IntelliJ*/) =>
      val idToOrder = persisted.aggregate.idToOrder
      enqueue:
        touchedOrderIds.distinct.flatMap:
          idToOrder.get

  private def enqueue(orderId: OrderId): IO[Unit] =
    withCurrentOrder(orderId): order =>
      enqueue(Vector(order))

  private def enqueue(orders: Seq[Order[Order.State]]): IO[Unit] =
    IO.whenA(orders.nonEmpty):
      orderQueue.offer(Some(orders))

  private def runOrderPipeline: IO[Unit] =
    // TODO Beliebige Aufträge sollen aus orderQueue löschbar sein
    fs2.Stream.fromQueueNoneTerminated(orderQueue)
      .chunks.map: chunk =>
        chunk.toVector.flatten
      .evalTapChunk: (orderIds: Vector[OrderId]) =>
        IO(bean.orderQueueLength -= orderIds.size)
      .through:
        orderPipeline
      .compile.drain

  private def orderPipeline: fs2.Pipe[IO, Seq[Order[Order.State]], Unit] =
    _.mapChunks: chunk =>
      Chunk:
        chunk.toVector.flatten.distinctBy(_.id)
    .evalMap:
      continueOrders

  private def continueOrders(orders: Vector[Order[Order.State]]): IO[Unit] =
    clock.lockIO: now =>
      //logger.whenTraceEnabled:
      //  orders.foreachWithBracket()((order, br) => logger.trace(s"### continueOrders:$br $order"))
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
      IO.whenA(orderIds.nonEmpty):
        journal.persist: agentState =>
          val keyedEvents = orderIds.flatMap: orderId =>
            OrderEventSource(agentState)(using InstructionExecutorService(clock))
              .nextEvents(orderId)
          //orderIds.filterNot(keyedEvents.view.map(_.key).toSet).foreachWithBracket(): (orderId, br) =>
          //  logger.trace(s"### triggered, but no event: $br ${agentState.idToOrder.getOrElse(orderId, orderId)}")
          keyedEvents
        .ifPersisted:
          onPersisted
        .map(_.orThrow) // TODO throws
        .flatMap: persisted =>
          jobMotorKeeper.onOrdersMayBeProcessable(orderIds, persisted.aggregate)

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
      orderQueue <- Resource.eval(Queue.unbounded[IO, Option[Seq[OrderId]]])
      bean <- registerMBean("OrderMotorMXBean", new Bean)
      orderMotor <- Service.resource:
        new OrderMotor(orderQueue, agentPath, subagentKeeper, journal, bean, agentConf)
      _ <- orderMotor.jobMotorKeeper.registerMBeans
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


  private sealed trait OrderMotorMXBean:
    def getOrderQueueLength: Int

  private final class Bean extends OrderMotorMXBean:
    val orderQueueLength = Atomic(0)
    def getOrderQueueLength = orderQueueLength.get
