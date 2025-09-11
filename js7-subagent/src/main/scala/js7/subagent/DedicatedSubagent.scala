package js7.subagent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.all.*
import fs2.Pipe
import fs2.concurrent.SignallingRef
import js7.base.catsutils.CatsEffectExtensions.{joinStd, left, onErrorOrCancel, right}
import js7.base.catsutils.CatsExtensions.flatMapSome
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.LogLevel.{Info, Warn}
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixutils.{AsyncMap, SimpleLock}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.Delayer
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.{Event, EventCalc, EventId}
import js7.data.job.{JobConf, JobKey}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems.{SubagentIdMismatchProblem, SubagentIsShuttingDownProblem, SubagentRunIdMismatchProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.CoupleDirector
import js7.data.subagent.SubagentEvent.{SubagentShutdown, SubagentShutdownStarted}
import js7.data.subagent.{SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.journal.{CommitOptions, MemoryJournal, Persisted}
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.subagent.DedicatedSubagent.*
import js7.subagent.OutErrStatistics
import js7.subagent.configuration.SubagentConf
import js7.subagent.job.JobDriver
import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.math.Ordered.orderingToOrdered

final class DedicatedSubagent private(
  val subagentId: SubagentId,
  val subagentRunId: SubagentRunId,
  val commandExecutor: SubagentCommandExecutor,
  val journal: MemoryJournal[SubagentState],
  val agentPath: AgentPath,
  val agentRunId: AgentRunId,
  val controllerId: ControllerId,
  jobLauncherConf: JobLauncherConf,
  subagentConf: SubagentConf,
  persistedQueue: PersistedQueue,
  fileValueState: FileValueState)
  (using ioRuntime: IORuntime)
extends Service.StoppableByRequest:
  protected type S = SubagentState

  private val jobKeyToJobDriver = AsyncMap.empty[JobKey, JobDriver]
  private val orderIdToJobDriver = AsyncMap.stoppable[OrderId, JobDriver]()
  private val stoppingLock = false
  private val orderToProcessing = AsyncMap.stoppable[OrderId, Processing]()
  //private val director = AsyncVariable(none[Allocated[IO, DirectorRegisterable]])
  private var _isUsed = false
  @volatile private var _dontWaitForDirector = false
  @volatile private var _shuttingDown = false
  private var stopSignal: Option[ProcessSignal] = Some(SIGTERM)
  private var stopDontWaitForDirector = false

  def isUsed: Boolean =
    _isUsed || isShuttingDown

  def isShuttingDown: Boolean =
    _shuttingDown

  protected def start =
    startService:
      untilStopRequested *>
        IO.defer:
          stopMe(stopSignal, stopDontWaitForDirector) *>
            IO(logger.info(s"$toString stopped"))

  private[subagent] def stop(signal: Option[ProcessSignal], dontWaitForDirector: Boolean)
  : IO[Unit] =
    IO.defer:
      stopSignal = signal
      stopDontWaitForDirector = dontWaitForDirector
      stop

  private def stopMe(signal: Option[ProcessSignal], dontWaitForDirector: Boolean): IO[Unit] =
    logger.debugIO:
      IO.defer:
        _dontWaitForDirector |= dontWaitForDirector
        _shuttingDown = true
        persistedQueue.persisting:
          journal.persist:
            SubagentShutdownStarted
        .ignoreProblem(Warn)
        .productR:
          stopAllOrders(signal, dontWaitForDirector)
        .productR:
          persistedQueue.persisting:
            journal.persist:
              SubagentShutdown
          .map(_.orThrow)
        .void

  private def stopAllOrders(signal: Option[ProcessSignal], dontWaitForDirector: Boolean): IO[Unit] =
    logWhileStopping:
      orderIdToJobDriver.stop.both:
        signal.foldMap:
          killAndStopAllJobs
      .productR:
        orderToProcessing.initiateStopWithProblem(SubagentIsShuttingDownProblem)
      .productR:
        if dontWaitForDirector then IO:
          for orderId <- orderToProcessing.unsafeToMap.keys.toVector.sorted do logger.warn:
            s"Shutdown: Agent Director has not yet acknowledged processing of $orderId"
        else
          awaitOrderAcknowledgements *>
            // Await process termination and DetachProcessedOrder commands
            orderToProcessing.whenStopped
              .logWhenItTakesLonger("Director-acknowledged Order processes")

  private def logWhileStopping(body: IO[Unit]): IO[Unit] =
    IO.defer:
      val orderCount = orderIdToJobDriver.size
      val sym = BlockingSymbol()
      if orderCount > 0 then
        sym.onInfo()
        logger.info(s"🟡 Stopping, waiting for $orderCount processes")
      Delayer.continually():
        IO:
          val orderIds = orderIdToJobDriver.unsafeToMap.keys
          sym.onInfo()
          logger.info(s"🟡 Stopping, still waiting for ${orderIds.size} processes: ${
            orderIds.toVector.sorted.mkStringLimited(3)}")
      .background.surround:
        body
      .productR:
        IO:
          if sym.relievedLogLevel >= Info then
            logger.info(s"🟢 All processes completed")

  def stopWorkflowJobs(workflow: Workflow): IO[Unit] =
    IO.defer:
      workflow.keyToJob.keys.toVector
        .flatMap(jobKeyToJobDriver.get)
        .parUnorderedTraverse:
          _.stop(SIGKILL/*just in case*/)
        .productR:
          val isOurs = workflow.keyToJob.keySet
          jobKeyToJobDriver.removeConditional((jobKey, _) => isOurs(jobKey))
        .void

  //private[subagent] def dedicateDirector(cmd: DedicateDirector, meta: CommandMeta)
  //: IO[Checked[Unit]] =
  //  director.value.flatMap {
  //    case Some(allo) =>
  //      allo.allocatedThing.dedicateDirector(cmd, meta)
  //
  //    case None =>
  //      director
  //        .updateChecked {
  //          case Some(allo) => IO.right(Some(allo))
  //          case None =>
  //            toDirector(cmd, meta)
  //              .flatMapT(_.toAllocated.map(allo => Right(Some(allo))))
  //        }
  //        .startAndForget
  //        .as(Left(AgentDirectorIsStartingProblem))
  //  }

  private[subagent] def executeCoupleDirector(cmd: CoupleDirector): IO[Checked[Unit]] =
    IO:
      for
        _ <- checkSubagentId(cmd.subagentId)
        _ <- checkSubagentRunId(cmd.subagentRunId)
        _ <- journal.eventWatch.checkEventId(cmd.eventId)
      yield ()
    .flatMapT: _ =>
      if cmd.eventId == EventId.BeforeFirst then
        IO.right(())
      else
        releaseEvents(cmd.eventId)

  private def checkSubagentId(requestedSubagentId: SubagentId): Checked[Unit] =
    (requestedSubagentId == subagentId) !!
      SubagentIdMismatchProblem(requestedSubagentId, subagentId)

  private[subagent] def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
    if requestedSubagentRunId != subagentRunId then
      val problem = SubagentRunIdMismatchProblem(subagentId)
      logger.warn:
        s"$problem, requestedSubagentRunId=$requestedSubagentRunId, " +
          s"agentRunId=${this.subagentRunId}"
      Left(problem)
    else
      Checked.unit

  private def awaitOrderAcknowledgements: IO[Unit] =
    logger.debugIO:
      IO.defer:
        val since = Deadline.now
        val oToP = orderToProcessing.unsafeToMap.toVector
        val sym = BlockingSymbol()
        List(0.s/*debug*/, 1.s/*info*/).foldMap: delay =>
          // Wait a second before logging at info level
          IO:
            sym.escalate()
            logger.log(sym.logLevel,
              s"$sym Delaying shutdown until Agent Director has acknowledged processing of ${
                oToP.map(_._1).sorted.mkStringLimited(3)}")
          .delayBy(delay)
        .background.surround:
          oToP.parFoldMapA: (orderId, processing) =>
            processing.acknowledged.get *>
              IO(logger.log(sym.relievedLogLevel,
                s"🟢 Director has acknowledged processing of $orderId"))
        .productR:
          IO(logger.log(sym.relievedLogLevel,
            s"🟢 Director has acknowledged processing of all orders after ${
              since.elapsed.pretty}"))

  private def killAndStopAllJobs(signal: ProcessSignal): IO[Unit] =
    logger.debugIO("killAndStopAllJobs", signal):
      IO(jobKeyToJobDriver.unsafeToMap.values)
        .flatMap(_
          .toVector
          .parFoldMapA: jobDriver =>
            jobDriver.stop(signal)
              .handleError(t => logger.error(s"Stop $jobDriver: ${t.toStringWithCauses}")))

  def startOrderProcess(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    IO.defer:
      if isStopping then
        IO.left(SubagentIsShuttingDownProblem)
      else
        _isUsed = true
        orderToProcessing.updateChecked(order.id):
          case Some(processing) =>
            if processing.order != order then
              val problem = Problem("Duplicate SubagentCommand.StartOrder with changed Order")
              logger.warn(s"${order.id}: $problem: ⏎")
              logger.warn(s"${order.id}  - incoming order: $order")
              logger.warn(s"${order.id}  - existing order: ${processing.order}")
              IO.left(problem)
            else
              // Operation is idempotent
              logger.debug(s"startOrderProcess ${order.id}: process has already been started")
              IO.right(processing)

          case None =>
            startOrderProcess2(order, executeDefaultArguments, endOfAdmissionPeriod)
              .flatTap: _ =>
                IO.whenA(_dontWaitForDirector):
                  logger.warn:
                    s"dontWaitForDirector: ${order.id} <-: OrderProcessed event may get lost"
                  orderToProcessing.remove(order.id).void
              .map: fiber =>
                Right:
                  new Processing(order, fiber)
    .onError: t =>
      logger.error(s"🔥 startOrderProcess ${order.id}: ${t.toStringWithCauses}", t)
      // Tidy-up on failure
      orderToProcessing.remove(order.id).void
    .onProblem: problem =>
      logger.debug(s"⚠️  startOrderProcess ${order.id}: $problem")
      // Tidy-up on failure
      orderToProcessing.remove(order.id).void
    .map(_.map(_.fiber))

  private def startOrderProcess2(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp])
  : IO[FiberIO[OrderProcessed]] =
    startOrderProcess3(order, executeDefaultArguments, endOfAdmissionPeriod)
      .flatMap(_
        .joinStd
        .handleError(OrderOutcome.Failed.fromThrowable)
        .flatMap: outcome =>
          val orderProcessed = OrderProcessed(outcome)
          if journal.isHalted then
            // We simulate !!!
            logger.debug(s"⚠️ $orderProcessed suppressed because journal is halted")
            IO.pure(orderProcessed)
          else
            // Lock this section to execute persistedQueue.enqueue in proper ascending order
            // TODO Allow concurrent persisting of multiple OrderProcessed
            persistedQueue.lock:
              journal.persistOne(order.id <-: orderProcessed)
                .map(_.orThrow._1)
                .flatMap: stamped =>
                  persistedQueue.enqueueProcessedOrderId(stamped.eventId, order.id)
                    .as(stamped.value.event)
        .start)

  private def startOrderProcess3(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp])
  : IO[FiberIO[OrderOutcome]] =
    jobDriver(order.workflowPosition).flatMap:
      case Left(problem) =>
        IO.pure(OrderOutcome.Disrupted(problem): OrderOutcome).start

      case Right((workflowJob, jobDriver)) =>
        val releaseAssignment = orderIdToJobDriver.remove(order.id).void
        orderIdToJobDriver
          .put(order.id, jobDriver)
          .productR:
            stdObserversResource(order, keepLastErrLine = workflowJob.failOnErrWritten)
              .allocated
          .flatMap: (stdObservers, releaseStdObservers) =>
            jobDriver
              .runOrderProcess(order, executeDefaultArguments, endOfAdmissionPeriod, stdObservers)
              .guarantee(releaseStdObservers)
              .guarantee(releaseAssignment)
              .start
              .onErrorOrCancel(releaseStdObservers)
          .onErrorOrCancel(releaseAssignment)
          .recoverWith:
            case ProblemException(problem) if orderIdToJobDriver.isStoppingWith(problem) =>
              val processLost = OrderOutcome.processLost(SubagentShutDownBeforeProcessStartProblem)
              IO.pure(processLost: OrderOutcome).start

  private def stdObserversResource(order: Order[Order.Processing], keepLastErrLine: Boolean)
  : ResourceIO[StdObservers] =
    import subagentConf.{outerrByteBufferSize, outerrQueueSize, stdouterr}
    for
      outErrStatistics <- OutErrStatistics.stdouterrToStatisticsResource
      stdObservers <- StdObservers.resource(
        outErrToJournalSink(order.id, outErrStatistics),
        byteBufferSize = outerrByteBufferSize,
        chunkSize = stdouterr.chunkSize,
        delay = stdouterr.delay,
        queueSize = outerrQueueSize,
        useErrorLineLengthMax = keepLastErrLine ? jobLauncherConf.errorLineLengthMax,
        name = s"${order.id} ${order.workflowPosition}")
    yield
      stdObservers

  def releaseEvents(eventId: EventId): IO[Checked[Unit]] =
    journal.releaseEvents(eventId).flatMapT: _ =>
      persistedQueue.waitUntil(eventId)
        .logWhenItTakesLonger(s"releaseEvents($eventId)")
        .productR:
          detachProcessedOrders(eventId)
        .map(Right(_))

  private def detachProcessedOrders(eventId: EventId): IO[Unit] =
    IO.uncancelable: _ =>
      persistedQueue.takeOrderIdsUntil(eventId).flatMap: orderIds =>
        orderIds.traverse: orderId =>
          orderToProcessing.remove(orderId).flatMapSome: processing =>
            processing.acknowledged.complete(()).as(orderId)
        .map: detachedOrderIds_ =>
          val detachedOrderIds = detachedOrderIds_.flatten: Seq[OrderId]
          if detachedOrderIds.isEmpty then
            logger.trace(s"🪱 detachProcessedOrders($eventId): no Order detached")
          else
            logger.debug(s"detachProcessedOrders($eventId): detached ${
              detachedOrderIds.mkString(" ")}")

  private val stdoutCommitDelayOptions = CommitOptions(
    commitLater = true,
    delay = subagentConf.stdoutCommitDelay)

  private def outErrToJournalSink(
    orderId: OrderId,
    outErrStatistics: Map[StdoutOrStderr, OutErrStatistics])
    (outErr: StdoutOrStderr)
  : Pipe[IO, String, Nothing] =
    _.chunks
      .map: chunk =>
        chunk.toVector.map: string =>
          orderId <-: OrderStdWritten(outErr)(string)
      .foreach: events =>
        val charCount = events.iterator.map(_.event.chunk.length).sum
        outErrStatistics(outErr).count(n = events.size, charCount = charCount):
          persistedQueue.persisting:
            journal.persist(stdoutCommitDelayOptions)(EventCalc.pure(events))
        .map:
          _.onProblem: problem =>
            logger.error(s"Emission of OrderStdWritten event failed: $problem")
        .void

  // Create the JobDriver if needed
  private def jobDriver(workflowPosition: WorkflowPosition)
  : IO[Checked[(WorkflowJob, JobDriver)]] =
    journal.aggregate.map: state =>
      for
        workflow <- state.idToWorkflow.checked(workflowPosition.workflowId)
        jobKey <- workflow.positionToJobKey(workflowPosition.position)
        workflowJob <- workflow.keyToJob.checked(jobKey)
      yield
        jobKeyToJobDriver
          .getOrElseUpdate(jobKey,
            IO.defer:
              val jobConf = JobConf(
                jobKey, workflowJob, workflow, controllerId,
                sigkillDelay = workflowJob.sigkillDelay
                  .getOrElse(subagentConf.defaultJobSigkillDelay),
                jobLauncherConf.systemEncoding)
              JobDriver.start(JobDriver.Params(
                jobConf,
                id => journal.unsafeAggregate() /*live!*/.pathToJobResource.checked(id),
                JobLauncher.checked(jobConf, jobLauncherConf),
                fileValueState)))
          .map(workflowJob -> _)
    .flatMap(_.sequence)

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    for
      maybeJobDriver <- IO(orderIdToJobDriver.get(orderId))
      _ <- maybeJobDriver
        .fold(IO(logger.debug(s"⚠️ killProcess $orderId => no JobDriver for Order"))):
          _.killProcess(orderId, signal)
    yield ()

  def longName: String =
    s"$subagentId in $agentPath for $controllerId"

  override def toString =
    s"DedicatedSubagent($longName)"


object DedicatedSubagent:
  private val logger = Logger[this.type]
  private val ShutdownOrderAckWorryDurations = List(0.s/*debug*/, 1.s/*info*/, 3.s, 6.s, 10.s)

  def service(
    subagentId: SubagentId,
    subagentRunId: SubagentRunId,
    commandExecutor: SubagentCommandExecutor,
    journal: MemoryJournal[SubagentState],
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId,
    jobLauncherConf: JobLauncherConf,
    subagentConf: SubagentConf)
    (using ioRuntime: IORuntime)
  : ResourceIO[DedicatedSubagent] =
    for
      _ <- OutErrStatistics.registerMXBean
      fileValueState <- Resource.fromAutoCloseable(IO:
        FileValueState(subagentConf.valueDirectory))
      service <- Service.resource:
        for
          eventIdSignal <- SignallingRef[IO].of(EventId.BeforeFirst)
          persistedQueue = PersistedQueue(eventIdSignal)
        yield
          DedicatedSubagent(
            subagentId, subagentRunId, commandExecutor, journal, agentPath, agentRunId, controllerId,
            jobLauncherConf, subagentConf, persistedQueue, fileValueState)
    yield
      service


  private final class Processing(
    val order: Order[Order.Processing]/*for check only*/,
    val fiber: FiberIO[OrderProcessed]):

    val acknowledged = Deferred.unsafe[IO, Unit]


  /** Links `OrderProcessed` events with its `EventId` such that `ReleaseEvents` can
    * release `Order`, too. */
  private final class PersistedQueue(eventIdSignal: SignallingRef[IO, EventId]):
    private val persistLock = SimpleLock[IO]
    private val queue = mutable.Queue.empty[(eventId: EventId, processedOrderId: OrderId)]

    def persisting[E <: Event](persist: IO[Checked[Persisted[SubagentState, E]]])
    : IO[Checked[Persisted[SubagentState, E]]] =
      lock:
        persist.ifPersisted: persisted =>
          enqueue(persisted.stampedKeyedEvents.last.eventId)

    private[DedicatedSubagent] def lock[A](io: IO[A]): IO[A] =
      persistLock.surround(io)

    private[DedicatedSubagent] def enqueueProcessedOrderId(
      eventId: EventId, processedOrderId: OrderId)
    : IO[Unit] =
      IO:
        val pair = (eventId, processedOrderId)
        queue.synchronized:
          queue.enqueue(pair)
      *>
        // Because persist has notified the Director's event reader,
        // a fast Director may do a ReleaseEvents command,
        // before we have enqueued the OrderProcessed EventId.
        // eventIdSignal synchronizes this.
        enqueue(eventId)

    private[DedicatedSubagent] def enqueue(eventId: EventId): IO[Unit] =
      eventIdSignal.set(eventId)

    /** Wait until `eventId` has been persisted.
      * @return OrderIds for enqueued EventIds of OrderProcessed event.
      */
    def waitUntil(eventId: EventId): IO[Unit] =
      eventIdSignal.waitUntil(_ >= eventId)

    /** @return OrderIds for enqueued EventIds of OrderProcessed event.
      */
    def takeOrderIdsUntil(eventId: EventId): IO[Vector[OrderId]] =
      IO:
        queue.synchronized:
          queue.dequeueWhile(_.eventId <= eventId).view.map(_.processedOrderId).toVector
