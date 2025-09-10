package js7.agent.motor

import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.{FiberIO, IO, ResourceIO}
import cats.syntax.flatMap.*
import fs2.Chunk
import fs2.concurrent.{Signal, SignallingRef}
import java.time.ZoneId
import js7.agent.data.AgentState
import js7.agent.motor.JobMotor.*
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, right, startAndForget}
import js7.base.catsutils.CatsEffectUtils.unlessDeferred
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.monixutils.SimpleLock
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.service.Problems.ServiceStoppedProblem
import js7.base.service.Service
import js7.base.time.{AlarmClock, TimeInterval, Timestamp}
import js7.base.utils.Assertions.assertIfStrict
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.execution.workflow.instructions.AdmissionTimeSwitcher
import js7.data.job.JobKey
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.OrderEvent.OrderProcessed
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
      runPipeline *>
        IO:
          val n = processCount.get
          if n > 0 then logger.debug(s"❗️processCount=$n when stopping")

  def recoverProcessingOrders(orders: Vector[Order[Order.Processing]])
  : IO[Seq[(OrderId, Checked[FiberIO[OrderProcessed]])]] =
    IO.defer:
      processCount += orders.size
      subagentKeeper.recoverProcessingOrders(orders)

  def enqueue(orders: Seq[Order[IsFreshOrReady]]): IO[Unit] =
    IO.whenA(orders.nonEmpty):
      //IO(orders.foreachWithBracket()((o, br) => logger.trace(s"### enqueue $br$o"))) *>
      queue.enqueue(orders) *>
        queueSignal.set(() => orders.map(_.id).mkString(" "))

  def trigger(reason: => Any): IO[Unit] =
    unlessDeferred(queue.isEmptyUnsafe):
      lazy val reason_ = reason.toString
      logger.trace(s"trigger($reason_)")
      queueSignal.set(() => reason_)

  def remove(orderId: OrderId, unnecessary: String = ""): IO[Boolean] =
    queue.lockForRemoval:
      queue.remove(orderId)
    .flatTap:
      IO.whenA(_):
        IO(logger.debug(s"$unnecessary$orderId removed from queue"))

  private def runPipeline: IO[Unit] =
    queueSignal.discrete.merge:
      admissionSignal.discrete.map(o => () => s"admission $o")
    .evalMap: signalReason =>
      dequeue.map(signalReason -> _)
    .filter: (signalReason, chunk) =>
      chunk.nonEmpty || locally:
        logger.trace:
          s"""🪱 runPipeline: No Order is processable despite signal "${signalReason()}""""
        false
    .map(_._2)
    .unchunks
    .evalMap: o =>
      startOrderProcess(o).startAndForget // TODO How to cancel this?
    .interruptWhenF(untilStopRequested)
    .compile.drain

  private val dequeue: IO[Chunk[OrderWithEndOfAdmission]] =
    IO.defer:
      if queue.isEmptyUnsafe/*fast lane*/ then
        emptyChunk
      else
        dequeue2

  private lazy val dequeue2: IO[Chunk[OrderWithEndOfAdmission]] =
    queue.lockForRemoval:
      meterDequeue:
        admissionSignal.get.flatMap: maybeTimeInterval =>
          val endOfAdmission = maybeTimeInterval match
            case Some(o: TimeInterval.Standard) => Some(o.end)
            case _ => None
          val onlyForcedAdmission = maybeTimeInterval.isEmpty

          Vector.newBuilder[OrderWithEndOfAdmission].tailRecM: builder =>
            if queue.isEmpty(onlyForcedAdmission) then
              IO.right(builder)
            else
              tryIncrementProcessCount.map: ok =>
                if ok then
                  val order = queue.dequeueNextOrder(onlyForcedAdmission)
                  Left(builder += OrderWithEndOfAdmission(order, endOfAdmission))
                else
                  Right(builder)
          .map: builder =>
            Chunk.from(builder.result)
    .uncancelable

  private def startOrderProcess(orderWithEndOfAdmission: OrderWithEndOfAdmission): IO[Unit] =
    import orderWithEndOfAdmission.{endOfAdmission, order}
    // SubagentKeeper ignores the Order when it has been concurrently changed
    subagentKeeper.processOrder(order, endOfAdmission)
      .catchIntoChecked
      .flatMap:
        case Right(None) =>
          // SubagentKeeper has already logged a 🪱 debug line.
          // TODO Try to avoid this, see SubagentKeeper processOrderAndForwardEvents
          decrementProcessCount(order.id, s"${order.id} is not processable")
        case Right(Some(order)) =>
          IO.unlessA(order.isState[Order.Processing]):
            // Unless SubagentKeeper emitted an OrderProcessingStarted (due to changed Order,
            // duplicate enqueued OrderId), then decrement the process counters here.
            // Otherwise, an OrderProcessed event will decrement.
            logger.trace(s"subagentKeeper.processOrder: ${order.id
              } status is not Processing, decrementing processCount")
            decrementProcessCount(order.id,
              s"Order's status is not Processing but ${order.workflowPosition} ${order.state}",
              unnecessary = "🪱")
        case Left(problem) =>
          handleFailedProcessStart(order, problem)

  private def handleFailedProcessStart(order: Order[IsFreshOrReady], problem: Problem): IO[Unit] =
    getAgentState.flatMap: agentState =>
      // This is an unexpected situation
      def msg = s"subagentKeeper.processOrder(${order.id}) $jobKey: $problem • $order"
      agentState.idToOrder.get(order.id) match
        case None =>
          logger.warn(s"subagentKeeper.processOrder: ${order.id} has been removed concurrently",
            problem.throwableIfStackTrace)
          if problem != UnknownKeyProblem("OrderId", order.id) then
            logger.warn(msg)
          IO.unit //??? decrementProcessCount(order.id)

        case Some(current) =>
          if order == current then
            logger.warn(msg, problem.throwableIfStackTrace)
            decrementProcessCount(order.id, "⚠️  processOrder failed")
          else
            IO:
              if !(problem is ServiceStoppedProblem) then
                // Maybe not a warning worth?
                logger.warn(msg, problem.throwableIfStackTrace)
                logger.warn(s"${order.id} has been changed concurrently, failed  : $order")
                logger.warn(s"${order.id} has been changed concurrently, existing: $current")

  def onOrdersProcessed(orderIds: Iterable[OrderId]): IO[Unit] =
    orderIds.foldMap: orderId =>
      remove(orderId, unnecessary = "🪱") /*Remove a maybe duplicate inserted order???*/
        *> decrementProcessCount(orderId, "OrderProcessed")

  private def tryIncrementProcessCount: IO[Boolean] =
    processCountLock.surround:
      IO:
        orderMotor.jobMotorKeeper.processLimits
          .tryIncrementProcessCount(processCount, workflowJob.processLimit)

  /** Triggers this JobMotor or all JobMotor when a process counter gets below its limit.
    * @return true iff process count was at processLimit. */
  private def decrementProcessCount(orderId: OrderId, reason: String, unnecessary: String = "")
  : IO[Unit] =
    processCountLock.surround:
      IO.defer:
        if reason != "OrderProcessed" then
          logger.trace:
            s"${unnecessary}decrementProcessCount: processCount=$processCount $orderId ($reason)"
        val n = processCount.decrementAndGet()
        assertIfStrict(n >= 0, s"processCount=$n is below zero")
        orderMotor.jobMotorKeeper.processLimits.decrementProcessCount.flatMap: triggered =>
          IO.whenA(!triggered && n == workflowJob.processLimit - 1):
            trigger(s"processCount=$n is below Job's processLimit=${workflowJob.processLimit}")

  override def toString = s"JobMotor($jobKey)"


private object JobMotor:
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
