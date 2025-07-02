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
import js7.base.catsutils.CatsEffectUtils.{unlessDeferred, whenDeferred}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.{AsyncMap, SimpleLock}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.{AdmissionTimeScheme, AlarmClock, NonEmptyTimeInterval, TimeInterval}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic, DuplicateKeyException}
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

  def createJobEntries(workflow: Workflow): IO[Unit] =
    val zoneId = ZoneId.of(workflow.timeZone.string) // throws on unknown time zone !!!
    workflow.keyToJob.filter(_._2.agentPath == agentPath).foldMap: (jobKey, job) =>
      jobToEntry.insert(jobKey, JobEntry(jobKey, job, zoneId, agentConf.findTimeIntervalLimit))
        .map(_.orThrow)

  def deleteEntriesForWorkflow(workflowId: WorkflowId): IO[Unit] =
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
      getAgentState.flatMap: agentState =>
        agentState.idToWorkflow.checked(order.workflowId)
          .map(workflow => workflow -> workflow.instruction(order.position))
          .match
            case Left(problem) =>
              logger.error(s"onOrderIsProcessable => $problem")
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
                  jobEntry.enqueue(order.id).flatMap: enqueued =>
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

  //private def tryStartProcessing(jobEntry: JobEntry): IO[Unit] =
  //  tryStartProcessing(jobEntry,               maybeTimeInterval match
  //                  case Some(o: TimeInterval.Standard) => Some(o.end)
  //                  case Some(TimeInterval.Always) | None/*impossible*/ => None)

  private def tryStartProcessing(jobEntry: JobEntry): IO[Unit] =
    import jobEntry.jobKey
    unlessDeferred(isStopping):
      logger.traceIO("tryStartProcessing", jobKey):
        jobEntry.onAdmissionTimeInterval:
          tryStartProcessing(jobEntry)
        .flatMap: maybeTimeInterval =>
          val endOfAdmissionPeriod = maybeTimeInterval match
            case Some(o: TimeInterval.Standard) => Some(o.end)
            case Some(TimeInterval.Always) => None
            case None => None /*impossible*/
          lock.lock:
            getAgentState.flatMap: agentState =>
              whenDeferred(jobEntry.isBelowProcessLimits(agentState)):
                fs2.Stream.evalSeq:
                  jobEntry.dequeueOrdersWhere(
                    convert = orderId =>
                      // Always check Order's existence.
                      // Order may have been enqueued after we have read our AgentState.
                      agentState.idToOrder.get(orderId).flatMap: order =>
                        order.checkedState[Order.IsFreshOrReady] match
                          case Left(problem) =>
                            logger.error(s"dequeueOrdersWhere: $problem • $order")
                            None
                          case Right(order) => Some(order),
                    predicate = order =>
                      if !order.isProcessable then
                        logger.error(s"startProcessing but Order is not processable: $order")
                        false
                      else
                        // TODO OrderQueue should only contain immediately processable Orders
                        order.forceJobAdmission || maybeTimeInterval.isDefined)
                .evalMap: order =>
                  jobEntry.incrementProcessCount *>
                    // subagentKeeper.processOrder blocks until a Subagent becomes available
                    subagentKeeper.processOrder(order, endOfAdmissionPeriod, onSubagentEvents(order.id))
                      .catchIntoChecked
                      .handleProblemWith: problem =>
                        IO(logger.error:
                          s"startOrderProcessing($jobKey) ${order.id}: $problem • $order")
                      .startAndForget
                .compile.drain

  private def agentProcessLimit(agentState: AgentState): Option[Int] =
    agentState.keyToItem(AgentRef).get(agentPath) match
      case None =>
        logger.debug("❓  Missing own AgentRef — assuming processLimit = 0")
        Some(0)
      case Some(agentRef) =>
        agentRef.processLimit


  private final class JobEntry(val jobKey: JobKey, val workflowJob: WorkflowJob, zoneId: ZoneId,
    findTimeIntervalLimit: FiniteDuration)
    (using Dispatcher[IO]):

    private val lock = SimpleLock[IO]
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
          queue.recoverProcessingOrder(order)

    def enqueue(orderId: OrderId): IO[Boolean] =
      lock.surround:
        IO:
          if !queue.isKnown(orderId) then
            queue.enqueue(orderId)
            true
          else
            false

    def dequeueOrdersWhere[A](convert: OrderId => Option[A], predicate: A => Boolean)
    : IO[Seq[A]] =
      lock.surround:
        IO:
          queue.dequeueWhere(convert, predicate)

    def incrementProcessCount: IO[Unit] =
      lock.surround:
        IO:
          processCount += 1
          jobMotor.agentProcessCount += 1

    def remove(orderId: OrderId): IO[Boolean] =
      lock.surround:
        IO:
          queue.isKnown(orderId) && locally:
            queue.remove(orderId)
            processCount -= 1
            jobMotor.agentProcessCount -= 1
            true

    def onAdmissionTimeInterval(onPermissionGranted: IO[Unit])(using AlarmClock)
    : IO[Option[NonEmptyTimeInterval]] =
      lock.surround:
        admissionTimeIntervalSwitch.updateAndCheck(onPermissionGranted)

    def isBelowProcessLimits(agentState: AgentState): IO[Boolean] =
      getAgentState.map: agentState =>
        agentProcessLimit(agentState).forall(agentProcessCount.get() < _) &&
          processCount.get() < workflowJob.processLimit

    override def toString = s"JobEntry($jobKey)"
  end JobEntry


object JobMotor:
  private val logger = Logger[this.type]


  private final class OrderQueue:
    private val queue = mutable.ListBuffer.empty[OrderId]
    private val queueSet = mutable.Set.empty[OrderId]
    private val inProcess = mutable.Set.empty[OrderId]

    def nonEmpty: Boolean = queue.nonEmpty

    def isKnown(orderId: OrderId): Boolean =
      queueSet.contains(orderId) || inProcess.contains(orderId)

    def dequeueWhere[A](convert: OrderId => Option[A], predicate: A => Boolean): Seq[A] =
      if queue.isEmpty then
        Nil
      else
        val builder = Vector.newBuilder[A]
        var i = 0
        while i < queue.length do
          val orderId = queue(i)
          convert(orderId) match
            case Some(a) if predicate(a) =>
              builder += a
              queue.remove(i)
              queueSet -= orderId
              inProcess += orderId
            case None =>
              queue.remove(i)
              queueSet -= orderId
            case _ =>
              // TODO Leaving OrderIds in queue can make this loop slow
              i += 1
        builder.result()

    def enqueue(orderId: OrderId): Unit =
      if inProcess(orderId) then throw new DuplicateKeyException(s"Duplicate $orderId")
      if queueSet contains orderId then throw new DuplicateKeyException(s"Duplicate $orderId")
      queue += orderId
      queueSet += orderId

    def recoverProcessingOrder(order: Order[Order.Processing]): Unit =
      inProcess += order.id

    def remove(orderId: OrderId): Unit =
      if !inProcess.remove(orderId) then
        val size = queue.size
        queue -= orderId
        if queue.size == size then
          logger.debug(s"❓OrderQueue: unknown $orderId")
        queueSet -= orderId

    override def toString =
      s"OrderQueue(${queue.size} orders, ${inProcess.size} in process)"
  end OrderQueue
