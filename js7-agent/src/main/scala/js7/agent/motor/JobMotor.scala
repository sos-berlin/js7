package js7.agent.motor

import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.{FiberIO, IO, ResourceIO}
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import fs2.Chunk
import fs2.concurrent.{Signal, SignallingRef}
import java.time.ZoneId
import js7.agent.data.AgentState
import js7.agent.motor.JobMotor.*
import js7.agent.motor.JobOrderQueue.End
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, right, startAndForget}
import js7.base.catsutils.CatsEffectUtils.unlessDeferred
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.monixutils.SimpleLock
import js7.base.problem.Checked.*
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.{AlarmClock, TimeInterval, Timestamp}
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.execution.workflow.instructions.AdmissionTimeSwitcher
import js7.data.job.JobKey
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.{OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.subagent.director.SubagentKeeper
import scala.concurrent.duration.FiniteDuration

private final class JobMotor private(
  val jobKey: JobKey,
  workflowJob: WorkflowJob,
  getAgentState: IO[AgentState],
  orderMotor: OrderMotor,
  subagentKeeper: SubagentKeeper[AgentState],
  queueSignal: SignallingRef[IO, () => String],
  admissionSignal: Signal[IO, Option[TimeInterval]])
extends Service.StoppableByRequest:

  private val logger = Logger.withPrefix[this.type](jobKey.toString)
  private val processCountLock = SimpleLock[IO]
  private val processCount = Atomic(0)
  private val queue = new JobOrderQueue

  def start =
    startService:
      runPipeline

  override def stop =
    super.stop

  def recoverProcessingOrders(orders: Vector[Order[Order.Processing]])
  : IO[Seq[(OrderId, Checked[FiberIO[OrderProcessed]])]] =
    IO.defer:
      processCount += orders.size
      orders.parTraverse: order =>
        subagentKeeper.recoverOrderProcessing(order, onSubagentEvents(order.id))
          .map(order.id -> _)

  def enqueue(orders: Seq[Order[IsFreshOrReady]]): IO[Unit] =
    IO.whenA(orders.nonEmpty):
      IO.whenA(isStrict):
        getAgentState.flatMap: agentState =>
          IO:
            orders.foreach: order =>
              if !agentState.isOrderProcessable(order) then
                val msg = s"Order in job queue is not isOrderProcessable: $order"
                logger.error(msg)
                throw new AssertionError(msg)
      *>
        queue.enqueue(orders) *>
        queueSignal.set(() => orders.map(_.id).mkString(" "))

  def trigger(reason: => Any): IO[Unit] =
    unlessDeferred(queue.isEmptyUnsafe/*fast lane, if empty*/):
      queueSignal.set(() => reason.toString)

  def remove(orderId: OrderId): IO[Unit] =
    queue.lockForRemoval:
      queue.remove(orderId)
    .map:
      if _ then
        logger.debug(s"$orderId removed from queue")

  private def runPipeline: IO[Unit] =
    admissionSignal.discrete.map(o => () => s"admission $o").merge:
      queueSignal.discrete
    .evalMap: signalReason =>
      dequeue
        .map(signalReason -> _)
    .filter: (signalReason, chunk) =>
      chunk.nonEmpty || locally:
        logger.trace(s"runPipeline: No Order is processable despite signal: ${signalReason()}")
        false
    .map(_._2)
    .unchunks
    .takeWhile(_ != End)
    .evalMap:
      case o: OrderWithEndOfAdmission =>
        startOrderProcess(o).startAndForget // TODO How to cancel this?
      case End => IO.unit // Cannot happen due takeWhile(_ != End)
    .interruptWhenF(untilStopRequested)
    .compile.drain

  private val dequeue: IO[Chunk[OrderWithEndOfAdmission | End]] =
    IO.defer:
      if queue.isEmptyUnsafe/*fast lane*/ then
        emptyChunk
      else
        dequeue2

  private lazy val dequeue2: IO[Chunk[OrderWithEndOfAdmission | End]] =
    meterDequeue:
      queue.lockForRemoval:
        getAgentState.flatMap: agentState =>
          admissionSignal.get.flatMap: maybeTimeInterval =>
            val endOfAdmission = maybeTimeInterval match
              case Some(o: TimeInterval.Standard) => Some(o.end)
              case _ => None
            val onlyForcedAdmission = maybeTimeInterval.isEmpty
            Vector.newBuilder[OrderWithEndOfAdmission | End].tailRecM: builder =>
              if queue.isEmpty(onlyForcedAdmission) then
                IO.right(builder)
              else
                tryIncrementProcessCount(agentState):
                  queue.dequeueNextOrder(onlyForcedAdmission).map:
                    case End => Some(End)
                    case o: Order[IsFreshOrReady @unchecked] =>
                      Some(OrderWithEndOfAdmission(o, endOfAdmission))
                .map:
                  case None => Right(builder)
                  case Some(End) => Right(builder += End)
                  case Some(o) => Left(builder += o)
            .map: builder =>
              Chunk.from(builder.result)

  private def tryIncrementProcessCount[A](agentState: AgentState)(body: => IO[Option[A]])
  : IO[Option[A]] =
    IO.defer:
      if processCount.get() < workflowJob.processLimit then
        orderMotor.jobMotorKeeper.processLimits.tryIncrementProcessCount(agentState):
          body
        .map: result =>
          if result.isDefined then
            processCount += 1
          result
      else
        IO.none

  private def startOrderProcess(orderWithEndOfAdmission: OrderWithEndOfAdmission): IO[Unit] =
    import orderWithEndOfAdmission.{endOfAdmission, order}
    // SubagentKeeper ignores the Order when it has been changed concurrently
    subagentKeeper.processOrder(order, endOfAdmission, onSubagentEvents(order.id))
      .catchIntoChecked
      .handleProblemWith: problem =>
        handleFailedProcessStart(order, problem)

  private def handleFailedProcessStart(order: Order[IsFreshOrReady], problem: Problem): IO[Unit] =
    IO.defer:
      getAgentState.flatMap: agentState =>
        def msg = s"subagentKeeper.processOrder(${order.id}) $jobKey: $problem • $order"
        agentState.idToOrder.get(order.id) match
          case None =>
            logger.warn(s"subagentKeeper.processOrder: ${order.id} has been removed concurrently",
              problem.throwableIfStackTrace)
            if problem != UnknownKeyProblem("OrderId", order.id) then
              logger.error(msg)
            decrementAgentAndProcessCount
          case Some(current) =>
            if order == current then
              logger.warn(msg)
              decrementAgentAndProcessCount
            else
              logger.error(msg)
              if order.isState[Order.Processing] then
                logger.error(s"${order.id} will stay Processing due to unknown error")
              else
                logger.error(s"${order.id} has been changed concurrently: $order")
              IO.unit

  private def onSubagentEvents(orderId: OrderId)
    (events: Iterable[OrderStarted | OrderProcessingStarted | OrderProcessed])
  : IO[Unit] =
    events.foldMap:
      case OrderStarted => IO.unit
      case _: OrderProcessingStarted =>
        orderMotor.jobMotorKeeper.maybeKillOrder(orderId)

      case _: OrderProcessed =>
        onOrderProcessed(orderId) *>
          orderMotor.trigger(orderId)

  private def onOrderProcessed(orderId: OrderId): IO[Unit] =
    remove(orderId) /*Remove a maybe duplicate inserted order???*/ *>
      decrementAgentAndProcessCount

  /** @return true iff process count equalled its limit. */
  private def decrementAgentAndProcessCount: IO[Unit] =
    processCountLock.surround:
      IO.defer:
        processCount.getAndDecrement()
        orderMotor.jobMotorKeeper.processLimits.decrementProcessCount.flatMap: agentWasLimited =>
          if agentWasLimited then
            orderMotor.jobMotorKeeper.triggerAllJobs("processCount below AgentRef#processLimit")
          else
            IO.unit

  override def toString = s"JobMotor($jobKey)"


private object JobMotor:
  private val logger = Logger[this.type]
  private val meterDequeue = CallMeter("JobMotor.dequeue")
  private val emptyChunk = IO.pure(Chunk.empty)

  def service(
    jobKey: JobKey,
    workflowJob: WorkflowJob,
    getAgentState: IO[AgentState],
    orderMotor: OrderMotor,
    subagentKeeper: SubagentKeeper[AgentState],
    zoneId: ZoneId,
    findTimeIntervalLimit: FiniteDuration)
    (using AlarmClock, Dispatcher[IO])
  : ResourceIO[JobMotor] =
    for
      queueSignal <- Resource.eval(SignallingRef[IO].of[() => String](() => "initial signal"))
      admissionSignal <- AdmissionTimeSwitcher.signalService(
        workflowJob.admissionTimeScheme, zoneId, findTimeIntervalLimit, jobKey)
      service <- Service.resource:
        new JobMotor(
          jobKey,
          workflowJob,
          getAgentState,
          orderMotor,
          subagentKeeper,
          queueSignal,
          admissionSignal)
    yield
      service


  private final case class OrderWithEndOfAdmission(
    order: Order[IsFreshOrReady],
    endOfAdmission: Option[Timestamp])
