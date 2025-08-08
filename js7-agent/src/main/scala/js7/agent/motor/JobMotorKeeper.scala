package js7.agent.motor

import cats.effect.std.Dispatcher
import cats.effect.{FiberIO, IO, ResourceIO}
import cats.syntax.option.*
import cats.syntax.parallel.*
import java.time.ZoneId
import java.util.concurrent.atomic.AtomicInteger
import javax.annotation.Nullable
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.motor.JobMotorKeeper.*
import js7.base.catsutils.CatsEffectExtensions.False
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.{AsyncMap, SimpleLock}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.system.MBeanUtils.registerMBean
import js7.base.time.AlarmClock
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.{Allocated, AsyncLock, Atomic}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.job.JobKey
import js7.data.order.Order.{IsFreshOrReady, Processing}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.workflow.{Workflow, WorkflowId}
import js7.subagent.director.SubagentKeeper
import scala.collection.View

/** The Keeper of all JobMotors. */
private final class JobMotorKeeper(
  agentPath: AgentPath,
  orderMotor: OrderMotor,
  subagentKeeper: SubagentKeeper[AgentState],
  getAgentState: IO[AgentState],
  agentConf: AgentConfiguration)
  (using AlarmClock, Dispatcher[IO]):

  private val jobToMotor = AsyncMap[JobKey, Allocated[IO, JobMotor]]
  private val lock = AsyncLock()

  def stop: IO[Unit] =
    jobToMotor.removeAll.flatMap:
      _.values.toVector.parFoldMapA:
        _.release

  def attachWorkflow(workflow: Workflow): IO[Unit] =
    val zoneId = ZoneId.of(workflow.timeZone.string) // throws on unknown time zone !!!
    workflow.keyToJob.filter(_._2.agentPath == agentPath).foldMap: (jobKey, workflowJob) =>
      JobMotor
        .service(jobKey, workflowJob, getAgentState, orderMotor, subagentKeeper, zoneId,
          findTimeIntervalLimit = agentConf.findTimeIntervalLimit)
        .toAllocated
        .flatMap: allocated =>
          jobToMotor.insert(jobKey, allocated)
            .map(_.orThrow)

  def detachWorkflow(workflowId: WorkflowId): IO[Unit] =
    jobToMotor.removeConditional: (jobKey, _) =>
      jobKey.workflowId == workflowId
    .flatMap:
      _.values.toVector.parFoldMapA:
        _.release

  def recoverProcessingOrders(orders: Vector[Order[Processing]], agentState: AgentState)
  : IO[Seq[(OrderId, Checked[FiberIO[OrderProcessed]])]] =
    logger.debugIO:
      IO.defer: // Access mutable variables in IO
        orders.flatMap: order =>
          orderToJobMotor(order, agentState).map(_ -> order)
        .groupMap(_._1)(_._2)
        .toVector
        .parTraverse: (jobMotor, orders) =>
          processLimits.forceIncreaseProcessCount(orders.size) *>
            jobMotor.recoverProcessingOrders(orders)
        .map(_.flatten)

  def onOrdersMayBeProcessable(orderIds: Seq[OrderId], agentState: AgentState): IO[Unit] =
    //orderIds.foreachWithBracket()((orderId, br) => logger.trace(s"### onOrdersMayBeProcessable $br$orderId"))
    orderIds.view.flatMap:
      agentState.idToOrder.get
    .flatMap:
      agentState.ifOrderProcessable
    .flatMap: order =>
      agentState.maybeJobKey(order.workflowPosition)
        .map(_ -> order)
    .toVector
    .groupMap(_._1)(_._2)
    .foldMap: (jobKey, orders) =>
      keyToJobMotor(jobKey).enqueue(orders)

  def onOrderDetached(orderId: OrderId, originalAgentState: AgentState): IO[Unit] =
    originalAgentState.idToOrder.get(orderId)
      .flatMap(_.ifState[IsFreshOrReady])
      .flatMap: order =>
        orderToJobMotor(order, originalAgentState)
      .foldMap:
        _.remove(orderId)

  def triggerAllJobs(reason: => Any): IO[Unit] =
    lazy val reason_ = reason.toString
    logger.traceIO("triggerAllJobs", reason_):
      IO.defer:
        // TODO Respect Order's priority
        jobMotors.foldMap:
          _.trigger(reason_)

  private def jobMotors: View[JobMotor] =
    jobToMotor.toMap.values.view.map(_.allocatedThing)

  object processLimits:
    private val _processCount = Atomic(0)
    private val processLimitLock = SimpleLock[IO]
    private var _processLimit = none[Option[Int]]

    def processLimit: Option[Int] =
      _processLimit.getOrElse:
        logger.debug("⚠️  processLimit (own AgentRef) is still unknown, assuming processLimit = 0")
        Some(0)

    def processCount: Int =
      _processCount.get

    def onProcessLimitChanged(newLimit: Option[Int]): IO[Unit] =
      IO.defer:
        val isAtLimitAndGetsBigger =
          processLimits.synchronized:
            val previous = _processLimit.getOrElse(0.some)
            _processLimit = newLimit.some
            val processCount = this.processCount
            val wasAtLimit = previous.exists(_ <= processCount)
            val getsBigger = newLimit.forall(processCount < _)
            logger.trace(s"onProcessLimitChanged previous=$previous new=$newLimit wasAtLimit=${
              wasAtLimit} getsBigger=$getsBigger processCount=$processCount")
            wasAtLimit & getsBigger
        IO.whenA(isAtLimitAndGetsBigger):
          triggerAllJobs:
            s"Agent process limit increased to ${newLimit.getOrElse("unlimited")}"
        .map(Right(_))

    def forceIncreaseProcessCount(n: Int): IO[Unit] =
      IO:
        _processCount += n

    def tryIncrementProcessCount(jobProcessCount: AtomicInteger, jobProcessLimit: Int): Boolean =
      jobProcessCount.get < jobProcessLimit &&
        processLimits.synchronized:
          processLimit.forall(_processCount.get < _) && locally:
            jobProcessCount += 1
            _processCount += 1
            true

    /** @return true iff JobMotors have been triggered because a process has become available. */
    def decrementProcessCount: IO[Boolean] =
      IO:
        processLimit.synchronized:
          val n = _processCount.decrementAndGet()
          if n < 0 then
            val msg = s"🔥 decrementProcessCount: processCount=$n is below zero"
            logger.error(msg)
            if isStrict then throw new AssertionError(msg)
          processLimit.fold(None): limit =>
            (n + 1 == limit) ? (n, limit)
      .flatMap:
        case Some((n, limit)) =>
          triggerAllJobs: // TODO Slow, may happen to often
            s"processCount=$n got below AgentRef processLimit=$limit"
          .as(true)
        case _ =>
          IO.False

    def agentProcessLimit(agentState: AgentState): Option[Int] =
      agentState.keyToItem(AgentRef).get(agentPath) match
        case None =>
          logger.debug("❓ Missing own AgentRef — assuming processLimit = 0")
          Some(0)
        case Some(agentRef) =>
          agentRef.processLimit

  def onSubagentEvents(startedOrderIds: Seq[OrderId], processedOrderIds: Seq[OrderId])
  : IO[Unit] =
    startedOrderIds.foldMap(maybeKillOrder) *>
      onOrderProcessed(processedOrderIds)

  private def onOrderProcessed(processedOrderIds: Seq[OrderId])
  : IO[Unit] =
    getAgentState.flatMap: agentState =>
      processedOrderIds.flatMap:
        agentState.idToOrder.get
      .flatMap: order =>
        agentState.maybeJobKey(order.workflowPosition)
          .map(_ -> order.id)
      .groupMap(_._1)(_._2)
      .foldMap: (jobKey, orders) =>
        jobToMotor.get(jobKey).map(_.allocatedThing).foldMap: jobMotor =>
          jobMotor.onOrdersProcessed(orders)

  private def maybeKillOrder(orderId: OrderId): IO[Unit] =
    withCurrentOrder(orderId): order =>
      order.ifState[Processing].foldMap: order =>
        order.mark match
          case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
            maybeKillOrder(order, kill)

          case Some(OrderMark.Suspending(SuspensionMode(_, Some(mode)))) =>
            maybeKillOrder(order, mode)

          case _ => IO.unit

  def maybeKillOrder(order: Order[Order.State], kill: CancellationMode.Kill): IO[Unit] =
    order.ifState[Processing].foldMap: order =>
      IO.whenA(kill.workflowPosition.forall(_ == order.workflowPosition)):
        // RemoteSubagentDriver.killProcess kills asynchronously and does not block.
        // This operation must not block, otherwise the whole OrderMotor pipeline would block !!!
        subagentKeeper.killProcess(
          order.id,
          if kill.immediately then SIGKILL else SIGTERM)

  private def orderToJobMotor(order: Order[Order.State], agentState: AgentState): Option[JobMotor] =
    agentState.maybeJobKey(order.workflowPosition).match
      case None =>
        if order.isState[Processing] then
          logger.error:
            s"${order.id} is Processing but there is no Job for instruction at ${
              order.workflowPosition}"
        None
      case o => o
    .flatMap: jobKey =>
      jobToMotor.get(jobKey) match
        case None =>
          logger.error(s"${order.id} is Processing but no JobMotor is registered for $jobKey")
          None
        case o => o

    .map(_.allocatedThing)

  private def keyToJobMotor(jobKey: JobKey): JobMotor =
    jobToMotor.checked(jobKey).map(_.allocatedThing).orThrow

  private def withCurrentOrder[A](orderId: OrderId)(body: Order[Order.State] => IO[Unit]): IO[Unit] =
    getAgentState.flatMap: agentState =>
      agentState.idToOrder.get(orderId).foldMap:
        body

  private val bean: JobMotorKeeperMXBean = new JobMotorKeeperMXBean:
    def getProcessCount = processLimits.processCount
    /** @return null means no limit. */
    @Nullable def getProcessLimit =
      processLimits.processLimit.fold(null.asInstanceOf[java.lang.Integer])(Int.box)

  def registerMBeans: ResourceIO[Unit] =
    registerMBean("JobMotorKeeper", bean)
      .map(_ => ())


object JobMotorKeeper:
  private val logger = Logger[this.type]

  sealed trait JobMotorKeeperMXBean:
    def getProcessCount: Int
    @Nullable def getProcessLimit: java.lang.Integer
