package js7.agent.motor

import cats.effect.kernel.Resource
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Ref, ResourceIO}
import cats.syntax.traverse.*
import js7.agent.command.AgentCommandToEventCalc
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
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
import js7.data.event.{Event, EventCalc, KeyedEvent}
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

  private object bean extends OrderMotorMXBean:
    // TODO Delayed orders are not counted because they are hold in fibers. Make a queue for this!
    val orderQueueLength = Atomic(0)
    def getOrderQueueLength = orderQueueLength.get

  protected def start =
    subagentKeeper.coupleWithOrderMotor(onSubagentEvents)
      .productR:
        recoverAgentRefAndJobs
      .productR:
        startService:
          run

  private def run: IO[Unit] =
    (untilStopRequested *> orderQueue.offer(None))
      .background.surround:
        runOrderPipeline
      .guarantee:
        IO:
          val n = jobMotorKeeper.processLimits.processCount
          if n > 0 then logger.debug(s"❗️processCount=$n when stopping")
      .guarantee:
        orderToEntry.removeAll.flatMap:
          _.values.foldMap:
            _.cancelSchedule
      .guarantee:
        jobMotorKeeper.stop

  override def stop: IO[Unit] =
    super.stop

  private def recoverAgentRefAndJobs: IO[Unit] =
    journal.aggregate.flatMap: agentState =>
      agentState.keyToItem(AgentRef).get(agentPath).foldMap: agentRef =>
        proceedWithItem(agentRef).map(_.orThrow)
      .productR:
        agentState.idToWorkflow.values.foldMap:
          jobMotorKeeper.startJobMotors

  /** Run this after AgentReady event. */
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
              agentState.orders.view
                .filter(_.workflowPath == workflowPathControl.workflowPath)
                .map(_.id)
                .toVector
            .map(Right(_))
        else
          IO.right(())

      case _ => IO.right(())

  private def onSubagentEvents(
    keyedEvents: Iterable[KeyedEvent[OrderProcessingStarted | OrderProcessed]])
  : IO[Unit] =
    val startedBuilder = Vector.newBuilder[OrderId]
    val processedBuilder = Vector.newBuilder[OrderId]
    keyedEvents.foreach:
      case KeyedEvent(orderId, _: OrderProcessingStarted) => startedBuilder += orderId
      case KeyedEvent(orderId, _: OrderProcessed) => processedBuilder += orderId
    val processedOrderIds = processedBuilder.result()
    val startedOrderIds = startedBuilder.result().filterNot(processedOrderIds.toSet)

    startedOrderIds.foldMap(jobMotorKeeper.maybeKillMarkedOrder) *>
      jobMotorKeeper.onOrdersProcessed(processedOrderIds) *>
      enqueue(processedOrderIds)

  private def onPersisted(persisted: Persisted[AgentState, Event])(using enc: sourcecode.Enclosing)
  : IO[Unit] =
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
        val orderIds = chunk.toVector.flatten: Vector[OrderId]
        bean.orderQueueLength -= orderIds.size
        orderIds
      .evalMap: orderIds =>
        continueOrders(orderIds.distinct)
      .compile.drain

  private def continueOrders(orderIds: Vector[OrderId]): IO[Unit] =
    //orderIds.foreachWithBracket()((orderId, br) => logger.trace(s"### continueOrders $br$orderId"))
    IO.whenA(orderIds.nonEmpty):
      clock.lockIO: delayNow =>
        // persist may be delayed a little by the journal, so `delayNow` may be in the past.
        // Any we call isDelayed/ifDelayed three times.
        // But this way, we are sure to use the proper current AgentState.
        // FIRST, persist non-delayed Orders //
        journal.persist:
          // TODO Try to include SubagentKeeper OrderProcessingStarted here to avoid a race!
          EventCalc.combineAll:
            orderIds.view.map: orderId =>
              EventCalc.multiple: (agentState: AgentState) =>
                if agentState.idToOrder.get(orderId).exists(o => !o.isDelayed(delayNow)) then
                  OrderEventSource.nextEvents(orderId, agentState)
                else
                  Nil
        .ifPersisted:
          onPersisted
        .map(_.orThrow) // TODO throws
        .flatMap: persisted =>
          val orders = orderIds.flatMap(persisted.aggregate.idToOrder.get)
          // SECOND, schedule delayed Orders //
          scheduleDelayedOrders:
            orders.flatMap: order =>
              order.ifDelayed(delayNow).map(order.id -> _)
          .productR:
            // Maybe start jobs //
            val nonDelayedOrders = orders.view.filterNot(_.isDelayed(delayNow))
            jobMotorKeeper.onOrdersMayBeProcessable(nonDelayedOrders, persisted.aggregate)

  private def scheduleDelayedOrders(orderIds: Iterable[(OrderId, Timestamp)]): IO[Unit] =
    fs2.Stream.iterable(orderIds).zipWithBracket().evalMap:
      case ((orderId, delayUntil), br) =>
        orderToEntry.getOrElseUpdate(orderId, IO.pure(OrderEntry(orderId))).flatMap: entry =>
          logger.debug(s"scheduleDelayedOrders $br$delayUntil $orderId")
          entry.schedule(delayUntil):
            enqueue(orderId)
    .compile.drain

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
      orderMotor <- Service.resource:
        new OrderMotor(orderQueue, agentPath, subagentKeeper, journal, agentConf)
      _ <- registerMBean[IO]("OrderMotor", orderMotor.bean)
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
      clock.scheduleIOAt(timestamp, s"OrderMotor $orderId"):
        io
      .flatMap: cancel =>
        cancelScheduleRef.getAndSet(cancel)
          .flatten/*cancel previous schedule*/


  private sealed trait OrderMotorMXBean:
    def getOrderQueueLength: Int
