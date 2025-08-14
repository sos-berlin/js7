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
import js7.base.system.MBeanUtils.registerMBean
import js7.base.time.{AlarmClock, Timestamp}
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.MultipleLinesBracket.zipWithBracket
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.command.CancellationMode
import js7.data.event.{Event, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.InventoryItem
import js7.data.order.OrderEvent.{OrderDetachable, OrderDetached, OrderForked, OrderKillingMarked, OrderProcessed, OrderProcessingStarted}
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentBundle, SubagentItem}
import js7.data.workflow.WorkflowPathControl
import js7.journal.{FileJournal, Persisted}
import js7.subagent.director.SubagentKeeper

private final class OrderMotor private(
  orderQueue: Queue[IO, Option[Seq[OrderId]]],
  agentPath: AgentPath,
  subagentKeeper: SubagentKeeper[AgentState],
  journal: FileJournal[AgentState],
  bean: Bean,
  agentConf: AgentConfiguration)
  (using
    clock: AlarmClock,
    dispatcher: Dispatcher[IO])
extends Service.StoppableByRequest:

  private given InstructionExecutorService = InstructionExecutorService(clock)
  private val agentCommandToEventCalc = AgentCommandToEventCalc(agentConf)
  private val orderToEntry = AsyncMap[OrderId, OrderEntry]
  private[motor] val jobMotorKeeper =
    new JobMotorKeeper(agentPath, this, subagentKeeper, journal.aggregate, agentConf)

  protected def start =
    subagentKeeper.coupleWithOrderMotor(onSubagentEvents) *>
      startService:
        untilStopRequested
          .productR:
            orderQueue.offer(None)
          .background.surround:
            runOrderPipeline
          .guarantee:
            val n = jobMotorKeeper.processLimits.processCount
            if n > 0 then logger.debug(s"processCount=$n")
            orderToEntry.toMap.values.foldMap(_.cancelSchedule)

  override def stop: IO[Unit] =
    super.stop

  def recoverAgentRef(agentRef: AgentRef): IO[Checked[Unit]] =
    proceedWithItem(agentRef)

  def recoverWorkflows(agentState: AgentState): IO[Unit] =
    agentState.idToWorkflow.values.foldMap:
      jobMotorKeeper.attachWorkflow

  def recoverOrders(agentState: AgentState): IO[Unit] =
    logger.debugIO:
      val orders = agentState.idToOrder.values.view
      val processingOrders = orders.flatMap(_.ifState[Order.Processing]).toVector
      val otherOrderIds = orders.filterNot(_.isState[Order.Processing]).map(_.id).toVector
      recoverProcessingOrders(processingOrders, agentState) *>
        enqueue(otherOrderIds)

  /** Continue processing Orders (running at a remote Subagent). */
  private def recoverProcessingOrders(
    orders: Vector[Order[Order.Processing]],
    agentState: AgentState)
  : IO[Unit] =
    logger.debugIO:
      jobMotorKeeper.recoverProcessingOrders(orders, agentState).flatMap: checkedFibers =>
        checkedFibers.foldMap: (orderId, checkedFiber) =>
          checkedFiber.traverse: fiber =>
            fiber.joinStd.flatMap: (_: OrderProcessed) =>
              enqueue(orderId)
            .handleErrorWith: t =>
              IO(logger.error(s"recoverOrderProcessing($orderId): ${t.toStringWithCauses}", t))
            .startAndForget
          .handleProblemWith: problem =>
            IO(logger.error(s"recoverOrderProcessing($orderId): $problem"))

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
        jobMotorKeeper.processLimits.onProcessLimitChanged(agentRef.processLimit)
          .map(Right(_))

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
          journal.aggregate.flatMap: agentState =>
            enqueue:
              agentState.orders.view // Slow !!!
                .filter(_.workflowPath == workflowPathControl.workflowPath)
                .map(_.id)
                .toVector
            .map(Right(_))
        else
          IO.right(())

      case _ => IO.right(())

  def onSubagentEvents(keyedEvents: Iterable[KeyedEvent[OrderProcessingStarted | OrderProcessed]])
  : IO[Unit] =
    val typeToEvent: Map[Int, Vector[OrderId]] =
      keyedEvents.toVector.groupMap {
        case KeyedEvent(_, event: OrderProcessingStarted) => 1
        case KeyedEvent(_, event: OrderProcessed) => 2
      }(_.key)
    val processedOrderIds = typeToEvent.getOrElse(2, Vector.empty)
    val startedOrderIds = typeToEvent.getOrElse(1, Vector.empty).filterNot(processedOrderIds.toSet)

    jobMotorKeeper.onSubagentEvents(startedOrderIds, processedOrderIds) *>
      enqueue(processedOrderIds)

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
                _.cancelSchedule
              .as(Vector.empty)

          case _ =>
            IO.pure(Vector.empty)
        .map: touchedOrderIds =>
          touchedOrderIds :+ orderId
      case _ =>
        IO.pure(Vector.empty)
    .flatMap: (touchedOrderIds: Seq[OrderId]/*IntelliJ*/) =>
      val idToOrder = persisted.aggregate.idToOrder
      enqueue(touchedOrderIds.distinct)

  private def enqueue(orderId: OrderId): IO[Unit] =
    enqueue(orderId :: Nil)

  private def enqueue(orderIds: Seq[OrderId]): IO[Unit] =
    IO.whenA(orderIds.nonEmpty):
      bean.orderQueueLength += orderIds.size
      orderQueue.offer(Some(orderIds))

  private def runOrderPipeline: IO[Unit] =
    fs2.Stream.fromQueueNoneTerminated(orderQueue)
      .chunks.map: chunk =>
        chunk.toVector.flatten
      .evalTapChunk: (orderIds: Vector[OrderId]) =>
        IO(bean.orderQueueLength -= orderIds.size)
      .through:
        orderPipeline
      .compile.drain

  private def orderPipeline: fs2.Pipe[IO, Vector[OrderId], Unit] =
    _.map: orderIds =>
      // The last duplicate wins. This makes sure that idToOrder contains the current Order value
      orderIds.reverse.distinct.reverse
    .evalMap:
      continueOrders

  private def continueOrders(orderIds: Vector[OrderId]): IO[Unit] =
    scheduleDelayedOrders(orderIds)
      .flatMap: orderIds =>
        //orderIds.foreachWithBracket()((orderId, br) => logger.trace(s"### continueOrders $br$orderId"))
        IO.whenA(orderIds.nonEmpty):
          journal.persist: agentState =>
            // TODO Don't use same AgentState for each iteration. Use EventCalc!
            orderIds.flatMap:
              OrderEventSource.nextEvents(_, agentState)
          .ifPersisted: persisted =>
            //logger.whenTraceEnabled:
            //  orderIds.filterNot(persisted.keyedEvents.map(_.key).toSet).foreachWithBracket(): (orderId, br) =>
            //    logger.trace(s"### triggered, but no event: $br${persisted.aggregate.idToOrder.getOrElse(orderId, orderId)}")
            onPersisted(persisted)
          .map(_.orThrow) // TODO throws
          .flatMap: persisted =>
            jobMotorKeeper.onOrdersMayBeProcessable(orderIds, persisted.aggregate)

  /** @return non-delayed OrderIds. */
  private def scheduleDelayedOrders(orderIds: Vector[OrderId]): IO[Vector[OrderId]] =
    journal.aggregate.flatMap: agentState =>
      clock.lockIO: now =>
        IO:
          orderIds.flatMap(agentState.idToOrder.get).map: order =>
            order.id -> order.maybeDelayedUntil.getOrElse(Timestamp.Epoch)
          .partition(_._2 <= now)
    .flatTap: (_, delayOrderIds) =>
      fs2.Stream.iterable(delayOrderIds).zipWithBracket().evalMap:
        case ((orderId, delayUntil), br) =>
          orderToEntry.getOrElseUpdate(orderId, IO.pure(OrderEntry(orderId))).flatMap: entry =>
            logger.debug(s"scheduleDelayedOrders $br$delayUntil $orderId")
            entry.schedule(delayUntil):
              enqueue(orderId)
      .compile.drain
    .map(_._1.map(_._1))

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
      bean <- registerMBean("OrderMotor", new Bean)
      orderMotor <- Service.resource:
        new OrderMotor(orderQueue, agentPath, subagentKeeper, journal, bean, agentConf)
      _ <- orderMotor.jobMotorKeeper.registerMBeans
    yield
      orderMotor


  private final class OrderEntry(orderId: OrderId):
    private val cancelScheduleRef = Ref.unsafe[IO, IO[Unit]](IO.unit)

    def cancelSchedule: IO[Unit] =
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
