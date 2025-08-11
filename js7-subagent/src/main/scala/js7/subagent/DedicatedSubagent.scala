package js7.subagent

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, FiberIO, IO, Outcome, ResourceIO}
import cats.syntax.all.*
import fs2.Pipe
import js7.base.catsutils.CatsEffectExtensions.{guaranteeExceptWhenSucceeded, joinStd, right}
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic}
import js7.core.command.CommandMeta
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.{EventCalc, EventId}
import js7.data.job.{JobConf, JobKey}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.Problems.{SubagentIdMismatchProblem, SubagentIsShuttingDownProblem, SubagentRunIdMismatchProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.CoupleDirector
import js7.data.subagent.SubagentEvent.SubagentShutdown
import js7.data.subagent.{SubagentCommand, SubagentId, SubagentRunId, SubagentState}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.journal.{CommitOptions, MemoryJournal}
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.subagent.DedicatedSubagent.*
import js7.subagent.OutErrStatistics
import js7.subagent.configuration.SubagentConf
import js7.subagent.job.JobDriver
import scala.collection.mutable
import scala.concurrent.duration.Deadline

final class DedicatedSubagent private(
  val subagentId: SubagentId,
  val subagentRunId: SubagentRunId,
  val commandExecutor: SubagentCommandExecutor,
  val journal: MemoryJournal[SubagentState],
  val agentPath: AgentPath,
  val agentRunId: AgentRunId,
  val controllerId: ControllerId,
  jobLauncherConf: JobLauncherConf,
  subagentConf: SubagentConf)
  (using ioRuntime: IORuntime)
extends Service.StoppableByRequest:
  protected type S = SubagentState

  private val fileValueState = new FileValueState(subagentConf.valueDirectory)
  private val jobKeyToJobDriver = AsyncMap.empty[JobKey, JobDriver]
  private val orderIdToJobDriver = AsyncMap.stoppable[OrderId, JobDriver]()
  private val stoppingLock = AsyncLock()
  private val orderToProcessing = AsyncMap.stoppable[OrderId, Processing]()
  private val orderProcessedQueue = mutable.Queue.empty[ProcessedQueueEntry]
  //private val director = AsyncVariable(none[Allocated[IO, DirectorRegisterable]])
  private var _isUsed = false
  @volatile private var _dontWaitForDirector = false
  private val shuttingDown = Atomic(false)

  def isUsed: Boolean =
    _isUsed || isShuttingDown

  def isShuttingDown: Boolean =
    shuttingDown.get()

  protected def start =
    startService:
      untilStopRequested *> terminate(Some(SIGTERM))

  private[subagent] def terminate(
    signal: Option[ProcessSignal],
    dontWaitForDirector: Boolean = false)
  : IO[Unit] =
    stoppingLock.lock:
      logger.debugIO:
        IO.defer:
          _dontWaitForDirector |= dontWaitForDirector
          val isShuttingDown = shuttingDown.getAndSet(true)
          IO.unlessA(isShuttingDown):
            IO.defer:
              val orderCount = orderIdToJobDriver.size
              if orderCount > 0 then logger.info(s"Stopping, waiting for $orderCount processes")
              orderIdToJobDriver.stop
                .both:
                  signal.fold(IO.unit):
                    killAndStopAllJobs
                .productR:
                  IO.blocking(fileValueState.close())
                .productR:
                  orderToProcessing.initiateStopWithProblem(SubagentIsShuttingDownProblem)
                .productR:
                  if dontWaitForDirector then IO:
                    for orderId <- orderToProcessing.toMap.keys.toVector.sorted do logger.warn:
                      s"Shutdown: Agent Director has not yet acknowledged processing of $orderId"
                  else
                    awaitOrderAcknowledgements *>
                      // Await process termination and DetachProcessedOrder commands
                      orderToProcessing.whenStopped
                        .logWhenItTakesLonger("Director-acknowledged Order processes")
                .productR:
                  journal.persist:
                    SubagentShutdown
                  .handleProblem: problem =>
                    logger.warn(s"SubagentShutdown: $problem")
                .void

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal): IO[Unit] =
    val jobKeySet = jobKeys.toSet
    jobKeys.toVector
      .flatMap(jobKeyToJobDriver.get)
      .parUnorderedTraverse:
        _.stop(signal)
      .productR:
        jobKeyToJobDriver.removeConditional((jobKey, _) => jobKeySet(jobKey))
      .void

  def executeCommand(numbered: Numbered[SubagentCommand], meta: CommandMeta)
  : IO[Checked[numbered.value.Response]] =
    commandExecutor.executeCommand(numbered, meta)

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
        val oToP = orderToProcessing.toMap.toVector
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
    logger.debugIO("killAndStopAllJobs", signal)(
      IO(jobKeyToJobDriver.toMap.values)
        .flatMap(_
          .toVector
          .parUnorderedTraverse(jobDriver => jobDriver
            .stop(signal)
            .handleError(t => logger.error(s"Stop $jobDriver: ${t.toStringWithCauses}")))
          .map(_.combineAll)))

  def startOrderProcess(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    IO.defer:
      _isUsed = true
      orderToProcessing.updateChecked(order.id):
        case Some(processing) =>
          IO.pure:
            if processing.workflowPosition != order.workflowPosition then
              val problem = Problem.pure:
                "Duplicate SubagentCommand.StartOrder with different Order position"
              logger.warn(s"$problem:")
              logger.warn(s"  Added order   : ${order.id} ${order.workflowPosition}")
              logger.warn(s"  Existing order: ${order.id} ${processing.workflowPosition}")
              Left(problem)
            else
              // Operation is idempotent
              logger.debug(s"startOrderProcess ${order.id}: process has already been started")
              Right(processing)

        case None =>
          startOrderProcess2(order, executeDefaultArguments, endOfAdmissionPeriod)
            .guaranteeCase:
              case Outcome.Succeeded(_) =>
                IO.whenA(_dontWaitForDirector):
                  logger.warn:
                    s"dontWaitForDirector: ${order.id} <-: OrderProcessed event may get lost"
                  orderToProcessing.remove(order.id).void
              case outcome => IO.unit
            .map: fiber =>
              Right:
                new Processing(order.workflowPosition, fiber)
      .onError: t =>
        logger.error(s"🔥 startOrderProcess ${order.id}: ${t.toStringWithCauses}", t)
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
            journal.persistSingle(order.id <-: orderProcessed)
              .map(_.orThrow._1)
              .flatMap: stamped =>
                IO:
                  val entry = ProcessedQueueEntry(stamped.eventId, order.id)
                  orderProcessedQueue.synchronized:
                    orderProcessedQueue += entry
                  stamped.value.event
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
          .*>(
            stdObserversResource(order, keepLastErrLine = workflowJob.failOnErrWritten)
              .allocated)
          .flatMap: (stdObservers, releaseStdObservers) =>
            jobDriver
              .runOrderProcess(order, executeDefaultArguments, endOfAdmissionPeriod, stdObservers)
              .guarantee(releaseStdObservers)
              .guarantee(releaseAssignment)
              .start
              .guaranteeExceptWhenSucceeded(releaseStdObservers)
          .guaranteeExceptWhenSucceeded(releaseAssignment)
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
      IO:
        orderProcessedQueue.synchronized:
          orderProcessedQueue.dequeueWhile(_.eventId <= eventId).toEagerSeq
        .map(_.orderId)
      .flatMap:
        detachProcessedOrders
      .map(Right(_))

  private def detachProcessedOrders(orderIds: Seq[OrderId]): IO[Unit] =
    orderIds.foldMap: orderId =>
      orderToProcessing.remove(orderId).flatMap:
        _.fold(IO.unit):
          _.acknowledged.complete(()).void

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

  def resource(
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
      service <- Service.resource:
        DedicatedSubagent(
          subagentId, subagentRunId, commandExecutor, journal, agentPath, agentRunId, controllerId,
          jobLauncherConf, subagentConf)
    yield
      service

  private final class Processing(
    val workflowPosition: WorkflowPosition /*for check only*/ ,
    val fiber: FiberIO[OrderProcessed]):
    val acknowledged = Deferred.unsafe[IO, Unit]

  private final case class ProcessedQueueEntry(eventId: EventId, orderId: OrderId)
