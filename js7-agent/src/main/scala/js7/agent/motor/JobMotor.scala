package js7.agent.motor

import cats.effect.std.Dispatcher
import cats.effect.{FiberIO, IO}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.time.ZoneId
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.motor.JobMotor.*
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, startAndForget}
import js7.base.catsutils.CatsEffectUtils.unlessDeferred
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.{AsyncMap, SimpleLock}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, NonEmptyTimeInterval, TimeInterval}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.execution.workflow.instructions.ExecuteAdmissionTimeSwitch
import js7.data.job.JobKey
import js7.data.order.OrderEvent.{OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.Execute
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
  onSubagentEvents: OrderId => Seq[OrderStarted | OrderProcessingStarted | OrderProcessed] => IO[Unit],
  isStopping: => Boolean,
  agentConf: AgentConfiguration)
  (using AlarmClock, Dispatcher[IO]):
  jobMotor =>

  private val jobToEntry = AsyncMap[JobKey, JobEntry]
  private val agentProcessCount = Atomic(0)
  private val lock = AsyncLock()

  def stop: IO[Unit] =
    jobToEntry.toMap.values.foldMap(_.stop)

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
            subagentKeeper.recoverOrderProcessing(order, onSubagentEvents(order.id))
              .catchIntoChecked
          .map(order.id -> _)

  def onOrderProcessed(orderId: OrderId): IO[Unit] =
    getAgentState.flatMap: agentState =>
      agentState.idToOrder.get(orderId).fold(IO.unit): order =>
        val jobEntry = orderToJobEntry(agentState, order.workflowPosition).orThrow
        jobEntry.remove(orderId) *>
          tryStartProcessing(jobEntry) *>
          tryStartProcessing // In case, agentProcessCount gets below agentProcessLimit TODO Don't check too often

  def onOrderIsProcessable(order: Order[Order.State]): IO[Unit] =
    unlessDeferred(isStopping):
      order.ifState[Order.IsFreshOrReady].fold(IO.unit): order =>
        getAgentState.flatMap: agentState =>
          agentState.idToWorkflow.checked(order.workflowId)
            .map(workflow => workflow -> workflow.instruction(order.position))
            .match
              case Left(problem) =>
                logger.error(s"onOrderIsProcessable(${order.id}) => $problem")
                IO.unit

              case Right((workflow, execute: Execute)) =>
                val checkedJobKey = execute match
                  case _: Execute.Anonymous => Right(workflow.anonymousJobKey(order.workflowPosition))
                  case o: Execute.Named => workflow.jobKey(order.position.branchPath, o.name) // defaultArguments are extracted later
                checkedJobKey
                  .flatMap(jobToEntry.checked)
                  .onProblem: problem =>
                    logger.error(s"Internal: onOrderIsProcessable(${order.id}) => $problem")
                  .fold(IO.unit): jobEntry =>
                    jobEntry.enqueue(order).flatMap: enqueued =>
                      IO.whenA(enqueued):
                        tryStartProcessing(jobEntry)

              case Right(_) => IO.unit

  private def orderToJobEntry(agentState: AgentState, workflowPosition: WorkflowPosition): Checked[JobEntry] =
    agentState.jobKey(workflowPosition).flatMap(jobToEntry.checked)

  def tryStartProcessing: IO[Unit] =
    IO.defer:
      // TODO Respect Order's priority
      jobToEntry.toMap.values.foldMap:
        tryStartProcessing

  private def tryStartProcessing(jobEntry: JobEntry): IO[Unit] =
    unlessDeferred(isStopping):
      jobEntry.onNextAdmissionTimeInterval:
        tryStartProcessing(jobEntry)
      .flatMap: maybeTimeInterval =>
        tryStartProcessing2(jobEntry, maybeTimeInterval)

  private def tryStartProcessing2(jobEntry: JobEntry, maybeTimeInterval: Option[TimeInterval])
  : IO[Unit] =
    import jobEntry.jobKey
    lock.lock:
      logger.traceIO("tryStartProcessing2", jobKey):
        getAgentState.flatMap: agentState =>
          fs2.Stream.eval:
            IO.defer:
              if jobEntry.isBelowProcessLimits(agentState) then
                jobEntry.dequeueNextOrderId(jobAdmits = maybeTimeInterval.isDefined)
              else
                IO.none
          .unNoneTerminate
          .evalMap: order =>
            if !order.isProcessable then
              IO(logger.error(s"❓Order in job queue is not processable: $order"))
            else
              jobEntry.incrementProcessCount.productR:
                // subagentKeeper.processOrder blocks until a Subagent becomes available
                val endOfAdmissionPeriod = maybeTimeInterval match
                  case Some(o: TimeInterval.Standard) => Some(o.end)
                  case Some(TimeInterval.Always) => None
                  case None | Some(TimeInterval.Never) => None /*impossible*/
                // SubagentKeeper ignores the Order when it has been changed concurrently
                subagentKeeper.processOrder(order, endOfAdmissionPeriod, onSubagentEvents(order.id))
                  .catchIntoChecked
                  .handleProblemWith: problem =>
                    IO(logger.error(s"startOrderProcessing($jobKey) ${order.id}: $problem • $order"))
                  .startAndForget
          .compile.drain

  private def agentProcessLimit(agentState: AgentState): Option[Int] =
    agentState.keyToItem(AgentRef).get(agentPath) match
      case None =>
        logger.debug("❓Missing own AgentRef — assuming processLimit = 0")
        Some(0)
      case Some(agentRef) =>
        agentRef.processLimit


  private final class JobEntry(val jobKey: JobKey, val workflowJob: WorkflowJob, zoneId: ZoneId,
    findTimeIntervalLimit: FiniteDuration)
    (using Dispatcher[IO]):

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
      lock.surround:
        IO:
          processCount += 1
          jobMotor.agentProcessCount += 1
          if order.forceJobAdmission then
            forceAdmissionQueue.recoverProcessingOrder(order)
          queue.recoverProcessingOrder(order)

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
            forceAdmissionQueue.dequeueNext()

    def incrementProcessCount: IO[Unit] =
      lock.surround:
        IO:
          processCount += 1
          jobMotor.agentProcessCount += 1

    def remove(orderId: OrderId): IO[Boolean] =
      lock.surround:
        IO:
          if forceAdmissionQueue.isKnown(orderId) then
            forceAdmissionQueue.remove(orderId)
            processCount -= 1
            jobMotor.agentProcessCount -= 1
          queue.isKnown(orderId) && locally:
            queue.remove(orderId)
            processCount -= 1
            jobMotor.agentProcessCount -= 1
            true

    def onNextAdmissionTimeInterval(onPermissionGranted: IO[Unit])(using AlarmClock)
    : IO[Option[NonEmptyTimeInterval]] =
      lock.surround:
        admissionTimeIntervalSwitch.updateAndCheck(onPermissionGranted)

    def isBelowProcessLimits(agentState: AgentState): Boolean =
      agentProcessLimit(agentState).forall(agentProcessCount.get() < _) &&
        processCount.get() < workflowJob.processLimit

    override def toString = s"JobEntry($jobKey)"
  end JobEntry


object JobMotor:
  private val logger = Logger[this.type]

  private final class OrderQueue:
    private val queue = mutable.ListBuffer.empty[Order[Order.IsFreshOrReady]]
    private val queueSet = mutable.Set.empty[OrderId]
    //private val inProcess = mutable.Set.empty[OrderId]

    def nonEmpty: Boolean =
      queue.nonEmpty

    def isKnown(orderId: OrderId): Boolean =
      queueSet.contains(orderId) //|| inProcess.contains(orderId)

    def dequeueNext(): Option[Order[Order.IsFreshOrReady]] =
      if queue.isEmpty then
        None
      else
        Some(queue.remove(0))

    def enqueue(order: Order[Order.IsFreshOrReady]): Boolean =
      !isKnown(order.id) && locally:
        queue += order
        queueSet += order.id
        true

    @deprecated // ???
    def recoverProcessingOrder(order: Order[Order.Processing]): Unit =
      () //inProcess += order.id

    def remove(orderId: OrderId): Unit =
      //if !inProcess.remove(orderId) then
        val size = queue.size
        queue.indexWhere(_.id == orderId) match
          case -1 => logger.debug(s"❓OrderQueue: unknown $orderId")
          case i =>
            queue.remove(i)
            queueSet -= orderId

    override def toString =
        s"OrderQueue(${queue.size} orders)" //, ${inProcess.size} in process)"
  end OrderQueue
