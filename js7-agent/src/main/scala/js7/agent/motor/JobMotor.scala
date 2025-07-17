package js7.agent.motor

import cats.effect.std.Dispatcher
import cats.effect.{FiberIO, IO, ResourceIO}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.motor.JobMotor.*
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, startAndForget}
import js7.base.catsutils.CatsEffectUtils.unlessDeferred
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.{AsyncMap, SimpleLock}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.service.Service
import js7.base.time.{AdmissionTimeScheme, AlarmClock, NonEmptyTimeInterval, TimeInterval}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.{AsyncLock, Atomic}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.execution.workflow.instructions.ExecuteAdmissionTimeSwitch
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Workflow, WorkflowId}
import js7.subagent.director.SubagentKeeper
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

final class JobMotor(
  agentPath: AgentPath,
  subagentKeeper: SubagentKeeper[AgentState],
  getAgentState: IO[AgentState],
  agentConf: AgentConfiguration)
  (using AlarmClock, Dispatcher[IO])
extends Service.StoppableByRequest:
  jobMotor =>

  private val jobToEntry = AsyncMap[JobKey, JobEntry]
  private val agentProcessCount = Atomic(0)
  private val lock = AsyncLock()
  private var wiredOnOrderProcessed: OrderId => IO[Unit] = null.asInstanceOf

  protected def start =
    startService:
      untilStopRequested *> stopMe

  private def stopMe: IO[Unit] =
    jobToEntry.toMap.values.foldMap(_.stop)
    // TODO Wait until all processing have stopped

  def wireOnOrderProcessed(onOrderProcessed: OrderId => IO[Unit]): Unit =
    wiredOnOrderProcessed = onOrderProcessed

  def attachWorkflow(workflow: Workflow): IO[Unit] =
    val zoneId = ZoneId.of(workflow.timeZone.string) // throws on unknown time zone !!!
    workflow.keyToJob.filter(_._2.agentPath == agentPath).foldMap: (jobKey, job) =>
      jobToEntry.insert(jobKey, JobEntry(jobKey, job, zoneId, agentConf.findTimeIntervalLimit))
        .map(_.orThrow)

  def detachWorkflow(workflowId: WorkflowId): IO[Unit] =
    IO.defer:
      jobToEntry.removeConditional: (jobKey, _) =>
        jobKey.workflowId == workflowId
      .void

  def recoverProcessingOrders(orders: Seq[Order[Order.Processing]])
  : IO[Seq[(OrderId, Checked[FiberIO[OrderProcessed]])]] =
    logger.debugIO:
      getAgentState.flatMap: agentState =>
        orders.traverse: order =>
          // Sequentially due to JobEntry lock
          agentState.jobKey(order.workflowPosition)
            .flatMap(jobToEntry.checked)
            .traverse: jobEntry =>
              jobEntry.recoverProcessingOrder(order).as(jobEntry)
            .map(order -> _)
        .flatMap:
          // Concurrently
          _.parTraverse: (order, checkedJobEntry) =>
            checkedJobEntry.flatTraverse: jobEntry =>
              subagentKeeper.recoverOrderProcessing(order, onSubagentEvents(order.id, jobEntry.jobKey))
                .catchIntoChecked
            .map(order.id -> _)

  def onOrderIsProcessable(order: Order[Order.State]): IO[Unit] =
    order.ifState[Order.IsFreshOrReady].foldMap: order =>
      getAgentState.flatMap: agentState =>
        agentState.maybeJobKey(order.workflowPosition)
          .flatMap(jobToEntry.get)
          .foldMap: jobEntry =>
            jobEntry.enqueue(order).flatMap: enqueued =>
              IO.whenA(enqueued):
                tryStartProcessing(jobEntry)

  def onOrderDetached(orderId: OrderId, originalAgentState: AgentState): IO[Unit] =
    originalAgentState.idToOrder.get(orderId)
      .flatMap(_.ifState[Order.IsFreshOrReady])
      .foldMap: order =>
        originalAgentState.maybeJobKey(order.workflowPosition)
          .flatMap(jobToEntry.get)
          .foldMap: jobEntry =>
            jobEntry.remove(orderId).map: isRemoved =>
              if isRemoved then
                logger.debug(s"$orderId removed from ${jobEntry.jobKey} queue")

  private def selectJobEntry(workflowPosition: WorkflowPosition, agentState: AgentState)
  : Option[JobEntry] =
    agentState.maybeJobKey(workflowPosition).flatMap(jobToEntry.get)

  def tryStartProcessingAllJobs: IO[Unit] =
    IO.defer:
      // TODO Respect Order's priority
      jobToEntry.toMap.values.foldMap:
        tryStartProcessing

  private def tryStartProcessing(jobEntry: JobEntry): IO[Unit] =
    IO.whenA(jobEntry.hasQueuedOrders):
      unlessDeferred(isStopping):
       logger.traceIO(s"### tryStartProcessing ${jobEntry.jobKey}"):
        jobEntry.onNextAdmissionTimeInterval:
          tryStartProcessing(jobEntry)
        .flatMap: maybeTimeInterval =>
          tryStartProcessing2(jobEntry, maybeTimeInterval)

  private def tryStartProcessing2(jobEntry: JobEntry, maybeTimeInterval: Option[TimeInterval])
  : IO[Unit] =
    import jobEntry.jobKey
    //lock.lock:
    jobEntry.xxLock.lock:
      getAgentState.flatMap: agentState =>
        IO.whenA(jobEntry.hasQueuedOrders):
          fs2.Stream.eval:
            IO.defer:
              if jobEntry.isBelowProcessLimits(agentState) then
                jobEntry.dequeueNextOrderId(jobAdmits = maybeTimeInterval.isDefined)
              else
                IO.none
          .unNoneTerminate
          .evalMap: order =>
            if !order.isProcessable then
              IO:
                val msg = s"Order in job queue is not processable: $order"
                logger.error(s"❓$msg")
                if isStrict then throw new AssertionError(msg)
            else
              jobEntry.incrementProcessCount.productR:
                // subagentKeeper.processOrder blocks until a Subagent becomes available
                val endOfAdmissionPeriod = maybeTimeInterval match
                  case Some(o: TimeInterval.Standard) => Some(o.end)
                  case _ => None
                // SubagentKeeper ignores the Order when it has been changed concurrently
                subagentKeeper
                  .processOrder(order, endOfAdmissionPeriod, onSubagentEvents(order.id, jobKey))
                  .catchIntoChecked
                  .handleProblemWith: problem =>
                    getAgentState.flatMap: agentState =>
                      val o = agentState.idToOrder.get(order.id)
                      val isEqual = o.contains(order)
                      val x = 1
                      IO(logger.error:
                        s"subagentKeeper.processOrder(${order.id}) $jobKey: $problem • $order")
                  .startAndForget
          .compile.drain

  private def onSubagentEvents(orderId: OrderId, jobKey: JobKey)
    (events: Iterable[OrderStarted | OrderProcessingStarted | OrderProcessed])
  : IO[Unit] =
    events.foldMap:
      case OrderStarted => IO.unit
      case _: OrderProcessingStarted => maybeKillOrder(orderId)
      case _: OrderProcessed =>
        onOrderProcessed(orderId, jobKey) *>
          wiredOnOrderProcessed(orderId)

  private def onOrderProcessed(orderId: OrderId, jobKey: JobKey): IO[Unit] =
    getAgentState.flatMap: agentState =>
      jobToEntry.checked(jobKey) match
        case Left(problem) =>
          IO(logger.error(s"❓ onOrderProcessed($orderId, $jobKey): $problem"))
        case Right(jobEntry) =>

          jobEntry.remove(orderId) /*TODO Delete this ?*/ *>
            jobEntry.decrementAgentAndProcessCount.flatMap: (agentWasLimited, jobWasLimited) =>
              if agentWasLimited then
                tryStartProcessingAllJobs
              else if jobWasLimited then
                tryStartProcessing(jobEntry)
              else
                IO.unit

  private def agentProcessLimit(agentState: AgentState): Option[Int] =
    agentState.keyToItem(AgentRef).get(agentPath) match
      case None =>
        logger.debug("❓ Missing own AgentRef — assuming processLimit = 0")
        Some(0)
      case Some(agentRef) =>
        agentRef.processLimit

  def maybeKillOrder(orderId: OrderId): IO[Unit] =
    withCurrentOrder(orderId): order =>
      order.ifState[Order.Processing].foldMap: order =>
        order.mark match
          case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
            maybeKillOrder(order, kill)

          case Some(OrderMark.Suspending(SuspensionMode(_, Some(mode)))) =>
            maybeKillOrder(order, mode)

          case _ => IO.unit

  def maybeKillOrder(order: Order[Order.State], kill: CancellationMode.Kill): IO[Unit] =
    order.ifState[Order.Processing].foldMap: order =>
      IO.whenA(kill.workflowPosition.forall(_ == order.workflowPosition)):
        subagentKeeper.killProcess(
          order.id,
          if kill.immediately then SIGKILL else SIGTERM)

  private def withCurrentOrder[A](orderId: OrderId)(body: Order[Order.State] => IO[Unit]): IO[Unit] =
    getAgentState.flatMap: agentState =>
      agentState.idToOrder.get(orderId).foldMap:
        body

  override def toString = "JobMotor"


  private final class JobEntry(val jobKey: JobKey, val workflowJob: WorkflowJob, zoneId: ZoneId,
    findTimeIntervalLimit: FiniteDuration)
    (using Dispatcher[IO]):

    val xxLock = AsyncLock(name = jobKey.toString)
    private val lock = SimpleLock[IO]
    private val forceAdmissionQueue = new OrderQueue
    private val queue = new OrderQueue
    private val admissionTimeIntervalSwitch = ExecuteAdmissionTimeSwitch(
      workflowJob.admissionTimeScheme.getOrElse(AdmissionTimeScheme.always),
      findTimeIntervalLimit,
      zoneId,
      onSwitch = to =>
        IO:
          if !to.contains(TimeInterval.Always) then
            logger.debug(s"$jobKey: Next admission: ${to getOrElse "None"} $zoneId"))

    private val processCount = Atomic(0)

    def stop: IO[Unit] =
      admissionTimeIntervalSwitch.cancelDelay

    def recoverProcessingOrder(order: Order[Order.Processing]): IO[Unit] =
      incrementProcessCount

    def enqueue(order: Order[Order.IsFreshOrReady]): IO[Boolean] =
      lock.surround:
        IO:
          if order.forceJobAdmission then
            forceAdmissionQueue.enqueue(order)
          queue.enqueue(order)

    def dequeueNextOrderId(jobAdmits: Boolean): IO[Option[Order[Order.IsFreshOrReady]]] =
      lock.surround:
        IO:
          if jobAdmits then
            val maybeOrder = queue.dequeueNext()
            maybeOrder.foreach: order =>
              forceAdmissionQueue.remove(order.id)
            maybeOrder
          else
            val maybeOrder = forceAdmissionQueue.dequeueNext()
            maybeOrder.foreach: order =>
              queue.remove(order.id)
            maybeOrder

    def incrementProcessCount: IO[Unit] =
      lock.surround:
        IO:
          jobMotor.agentProcessCount += 1
          processCount += 1

    /** @return true iff process count equalled its limit. */
    def decrementAgentAndProcessCount: IO[(Boolean, Boolean)] =
      lock.surround:
        IO.defer:
          val j = processCount.getAndDecrement()
          val jobWasLimited = j == processCount.getAndDecrement()

          val a = jobMotor.agentProcessCount.getAndDecrement()
          getAgentState.map: agentState =>
            // Agent process equalled its limit?
            val agentWasLimited = agentProcessLimit(agentState).forall(a == _)
            (agentWasLimited, jobWasLimited)

    def remove(orderId: OrderId): IO[Boolean] =
      lock.surround:
        IO:
          queue.remove(orderId) && locally:
            forceAdmissionQueue.remove(orderId)
            true

    def onNextAdmissionTimeInterval(onPermissionGranted: IO[Unit])(using AlarmClock)
    : IO[Option[NonEmptyTimeInterval]] =
      lock.surround:
        admissionTimeIntervalSwitch.updateAndCheck(onPermissionGranted)

    def isBelowProcessLimits(agentState: AgentState): Boolean =
      processCount.get() < workflowJob.processLimit
        && agentProcessLimit(agentState).forall(agentProcessCount.get() < _)

    def hasQueuedOrders: Boolean =
      queue.nonEmpty

    override def toString = s"JobEntry($jobKey)"
  end JobEntry


object JobMotor:
  private val logger = Logger[this.type]

  def service(
    agentPath: AgentPath,
    subagentKeeper: SubagentKeeper[AgentState],
    getAgentState: IO[AgentState],
    agentConf: AgentConfiguration)
    (using AlarmClock, Dispatcher[IO])
  : ResourceIO[JobMotor] =
    for
      jobMotor <- Service.resource:
        new JobMotor(agentPath, subagentKeeper, getAgentState, agentConf)
    yield
      jobMotor

  private final class OrderQueue:
    private val queue = mutable.ListBuffer.empty[Order[Order.IsFreshOrReady]]
    // isQueued is for optimisation
    private val isQueued = mutable.Set.empty[OrderId]

    def nonEmpty: Boolean =
      queue.nonEmpty

    def dequeueNext(): Option[Order[Order.IsFreshOrReady]] =
      queue.nonEmpty ? queue.remove(0)

    def enqueue(order: Order[Order.IsFreshOrReady]): Boolean =
      !isQueued(order.id) && locally:
        queue += order
        isQueued += order.id
        true

    def remove(orderId: OrderId): Boolean =
      isQueued.remove(orderId) && locally:
        queue.indexWhere(_.id == orderId) match
          case -1 => false
          case i => queue.remove(i); true

    override def toString =
        s"OrderQueue(${queue.size} orders)" //, ${inProcess.size} in process)"
  end OrderQueue
