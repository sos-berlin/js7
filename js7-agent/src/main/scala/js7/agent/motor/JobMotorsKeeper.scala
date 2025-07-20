package js7.agent.motor

import cats.effect.std.Dispatcher
import cats.effect.{FiberIO, IO}
import cats.syntax.parallel.*
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.motor.JobMotorsKeeper.*
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.problem.Checked.*
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

/** Manages the JobMotor·s. */
private final class JobMotorsKeeper(
  agentPath: AgentPath,
  orderMotor: OrderMotor,
  subagentKeeper: SubagentKeeper[AgentState],
  getAgentState: IO[AgentState],
  agentConf: AgentConfiguration)
  (using AlarmClock, Dispatcher[IO]):

  private val jobToMotor = AsyncMap[JobKey, Allocated[IO, JobMotor]]
  private val lock = AsyncLock()

  def stop: IO[Unit] =
    IO.defer:
      if jobToMotor.nonEmpty then
        logger.warn("JobMotorsKeeper stop, but Workflows have not been detached")
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
    orderIds.view.flatMap:
      agentState.idToOrder.get
    .flatMap:
      agentState.ifOrderProcessable
    .flatMap: order =>
      agentState.maybeJobKey(order.workflowPosition)
        .map(_ -> order)
    .toVector
    .groupMap(_._1)(_._2)
    .view.map: (jobKey, orders) =>
      keyToJobMotor(jobKey) -> orders
    .foldMap: (jobMotor: JobMotor, orders) =>
      jobMotor.enqueue(orders)

  def onOrderDetached(orderId: OrderId, originalAgentState: AgentState): IO[Unit] =
    originalAgentState.idToOrder.get(orderId)
      .flatMap(_.ifState[IsFreshOrReady])
      .flatMap: order =>
        orderToJobMotor(order, originalAgentState)
      .foldMap:
        _.remove(orderId)

  // TODO Slow!
  def tryStartProcessingAllJobs: IO[Unit] =
    IO.defer:
      // TODO Respect Order's priority
      jobToMotor.toMap.values.map(_.allocatedThing).foldMap:
        _.trigger

  object processLimits:
    private val agentProcessCount = Atomic(0)
    private val processLimitLock = AsyncLock()

    def forceIncreaseProcessCount(n: Int): IO[Unit] =
      IO:
        agentProcessCount += n

    def tryIncrementProcessCount[A](agentState: AgentState)(body: => IO[Option[A]]): IO[Option[A]] =
      processLimitLock.lock:
        if agentProcessLimit(agentState).forall(agentProcessCount.get < _) then
          agentProcessCount += 1
          body
        else
          IO.none

    /** @return true iff processCount was at current AgentRef#processLimit. */
    def decrementProcessCount: IO[Boolean] =
      getAgentState.map: agentState =>
        val was = agentProcessCount.getAndDecrement()
        if was <= 0 then
          val msg = s"🔥 agentProcessCount got negative: ${was - 1}"
          logger.error(msg)
          if isStrict then throw new AssertionError(msg)
        agentProcessLimit(agentState).forall(was >= _)

    private def agentProcessLimit(agentState: AgentState): Option[Int] =
      agentState.keyToItem(AgentRef).get(agentPath) match
        case None =>
          logger.debug("❓ Missing own AgentRef — assuming processLimit = 0")
          Some(0)
        case Some(agentRef) =>
          agentRef.processLimit

  def maybeKillOrder(orderId: OrderId): IO[Unit] =
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


object JobMotorsKeeper:
  private val logger = Logger[this.type]
