package js7.subagent

import cats.effect.kernel.Deferred
import cats.effect.kernel.Resource.ExitCase
import cats.effect.std.CyclicBarrier
import cats.effect.unsafe.IORuntime
import cats.effect.{Fiber, FiberIO, IO, Outcome, Resource, ResourceIO}
import cats.syntax.all.*
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.{guaranteeExceptWhenSucceeded, joinStd}
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.io.process.{ProcessSignal, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service
import js7.base.stream.Numbered
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic}
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
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
import js7.journal.CommitOptions
import js7.journal.state.Journal
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.subagent.DedicatedSubagent.*
import js7.subagent.configuration.SubagentConf
import scala.util.chaining.scalaUtilChainingOps

final class DedicatedSubagent private(
  val subagentId: SubagentId,
  val subagentRunId: SubagentRunId,
  val commandExecutor: SubagentCommandExecutor,
  val journal: Journal[SubagentState],
  val agentPath: AgentPath,
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
  //private val director = AsyncVariable(none[Allocated[IO, DirectorRegisterable]])
  @volatile private var _dontWaitForDirector = false
  private val shuttingDown = Atomic(false)

  def isLocal = true

  def isShuttingDown: Boolean =
    shuttingDown.get()

  protected def start =
    startService(
      untilStopRequested *>
        //director
        //  .use {
        //  case None => IO.unit
        //  case Some(allocated) => allocated.release
        //} *>
        terminate(Some(SIGTERM)))

  private[subagent] def terminate(
    signal: Option[ProcessSignal],
    dontWaitForDirector: Boolean = false)
  : IO[Unit] =
    stoppingLock.lock(IO.defer {
      _dontWaitForDirector |= dontWaitForDirector
      val first = !shuttingDown.getAndSet(true)
      IO
        .whenA(first)(IO.defer {
          val orderCount = orderIdToJobDriver.toMap.size
          if orderCount > 0 then {
            logger.info(s"Stopping, waiting for $orderCount processes")
          }
          IO
            .both(
              orderIdToJobDriver.stop,
              signal.fold(IO.unit)(killAndStopAllJobs))
            .*>(IO {
              fileValueState.close()
            })
            .*>(orderToProcessing.initiateStopWithProblem(SubagentIsShuttingDownProblem))
            .*>(IO.defer {
              if dontWaitForDirector then IO {
                for orderId <- orderToProcessing.toMap.keys.toVector.sorted do logger.warn(
                  s"Shutdown: Agent Director has not yet acknowledged processing of $orderId")
              } else
                awaitOrderAcknowledgements *>
                  // Await process termination and DetachProcessedOrder commands
                  orderToProcessing.whenStopped
                    .logWhenItTakesLonger("Director-acknowledged Order processes")
            })
            .*>(journal
              // The event may get lost due to immediate shutdown !!!
              .persistKeyedEvent(NoKey <-: SubagentShutdown)
              .rightAs(())
              .map(_.onProblemHandle(problem => logger.warn(s"SubagentShutdown: $problem"))))
        })
    })

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal) =
    val jobKeySet = jobKeys.toSet
    jobKeys.toVector
      .flatMap(jobKeyToJobDriver.get)
      .parUnorderedTraverse(_.stop(signal))
      .*>(jobKeyToJobDriver.removeConditional {
        case (jobKey, _) => jobKeySet(jobKey)
      })
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

  private def checkSubagentId(requestedSubagentId: SubagentId): Checked[Unit] =
    (requestedSubagentId == subagentId) !!
      SubagentIdMismatchProblem(requestedSubagentId, subagentId)

  private[subagent] def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
    if requestedSubagentRunId != subagentRunId then
      val problem = SubagentRunIdMismatchProblem(subagentId)
      logger.warn(
        s"$problem, requestedSubagentRunId=$requestedSubagentRunId, " +
          s"agentRunId=${this.subagentRunId}")
      Left(problem)
    else
      Checked.unit

  private def awaitOrderAcknowledgements: IO[Unit] =
    IO.defer:
      val oToP = orderToProcessing.toMap.toVector
      for orderId <- oToP.map(_._1).sorted do logger.info(
        s"🟡 Delaying shutdown until Agent Director has acknowledged processing of $orderId")
      oToP
        .parTraverse { case (orderId, processing) =>
          processing.acknowldeged.get
            .flatMap(_ =>
              IO(logger.info(s"🟢 Director has acknowledged processing of $orderId")))
        }
        .map(_.combineAll)

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
    executeDefaultArguments: Map[String, Expression])
  : IO[Checked[FiberIO[OrderProcessed]]] =
    IO.defer:
      orderToProcessing
        .updateChecked(order.id, {
          case Some(existing) =>
            IO.pure(
              if existing.workflowPosition != order.workflowPosition then {
                val problem = Problem.pure(
                  "Duplicate SubagentCommand.StartOrder with different Order position")
                logger.warn(s"$problem:")
                logger.warn(s"  Added order   : ${order.id} ${order.workflowPosition}")
                logger.warn(s"  Existing order: ${order.id} ${existing.workflowPosition}")
                Left(problem)
              } else
                Right(existing)) // Idempotency: Order process has already been started

          case None =>
            startOrderProcess2(order, executeDefaultArguments)
              .guaranteeCase {
                case Outcome.Succeeded(_) =>
                  IO.whenA(_dontWaitForDirector) {
                    logger.warn(
                      s"dontWaitForDirector: ${order.id} <-: OrderProcessed event may get lost")
                    orderToProcessing.remove(order.id).void
                  }

                case _ => orderToProcessing.remove(order.id).void // Tidy-up on failure
              }
              .map(fiber => Right(new Processing(order.workflowPosition, fiber)))
        })
        .map(_.map(_.fiber))

  private def startOrderProcess2(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression])
  : IO[FiberIO[OrderProcessed]] =
    startOrderProcess3(order, executeDefaultArguments)
      .flatMap(_
        .joinStd
        .handleError(OrderOutcome.Failed.fromThrowable)
        .flatMap { outcome =>
          val orderProcessed = OrderProcessed(outcome)
          if journal.isHalted then {
            // We simulate !!!
            logger.debug(s"⚠️  $orderProcessed suppressed because journal is halted")
            IO.pure(orderProcessed)
          } else
            journal
              .persistKeyedEvent(order.id <-: orderProcessed)
              .map(_.orThrow._1.value.event)
        }
        .start)

  private def startOrderProcess3(
    order: Order[Order.Processing],
    executeDefaultArguments: Map[String, Expression])
  : IO[FiberIO[OrderOutcome]] =
    jobDriver(order.workflowPosition).flatMap:
      case Left(problem) =>
        IO.pure(OrderOutcome.Disrupted(problem): OrderOutcome).start

      case Right((workflowJob, jobDriver)) =>
        val releaseAssignment = orderIdToJobDriver.remove(order.id).void
        orderIdToJobDriver
          .put(order.id, jobDriver)
          .*>(
            stdObserversResource(order.id, keepLastErrLine = workflowJob.failOnErrWritten)
              .allocated)
          .flatMap { case (stdObservers, releaseStdObservers) =>
            jobDriver.startOrderProcess(order, executeDefaultArguments, stdObservers)
              .flatMap(_
                .joinStd // Process terminated
                .guarantee(releaseStdObservers)
                .guarantee(releaseAssignment)
                .start
                .guaranteeExceptWhenSucceeded(releaseStdObservers))
          }
          .guaranteeExceptWhenSucceeded(releaseAssignment)
          .recoverWith:
            case ProblemException(problem) if orderIdToJobDriver.isStoppingWith(problem) =>
              val processLost = OrderOutcome.processLost(SubagentShutDownBeforeProcessStartProblem)
              IO.pure(processLost: OrderOutcome).start

  private def stdObserversResource(orderId: OrderId, keepLastErrLine: Boolean)
  : Resource[IO, StdObservers] =
    import subagentConf.{outerrCharBufferSize, outerrQueueSize}
    for
      stdObservers <- StdObservers.resource(
        charBufferSize = outerrCharBufferSize,
        queueSize = outerrQueueSize,
        useErrorLineLengthMax = keepLastErrLine ? jobLauncherConf.errorLineLengthMax)
      outErrStatistics <- outErrStatisticsResource
      _ <- copyOutErrToJournalInBackground(orderId, stdObservers, outErrStatistics)
    yield
      stdObservers

  /** Logs some stdout and stderr statistics. */
  private def outErrStatisticsResource: Resource[IO, Map[StdoutOrStderr, OutErrStatistics]] =
    Resource
      .make(
        acquire = IO:
          Map[StdoutOrStderr, OutErrStatistics](
            Stdout -> new OutErrStatistics,
            Stderr -> new OutErrStatistics))(
        release = outErrStatistics => IO:
          if outErrStatistics(Stdout).isRelevant || outErrStatistics(Stderr).isRelevant then
            logger.debug(
              s"stdout: ${outErrStatistics(Stdout)}, stderr: ${outErrStatistics(Stderr)}"))

  private def copyOutErrToJournalInBackground(
    orderId: OrderId,
    stdObservers: StdObservers,
    outErrStatistics: Map[StdoutOrStderr, OutErrStatistics])
  : ResourceIO[Unit] =
    Resource.suspend:
      CyclicBarrier[IO](3).map: startBarrier =>

        def writeStreamAsEvents(outerr: StdoutOrStderr, stream: Stream[IO, String]) =
          stream
            .onStart:
              startBarrier.await
            // Collect multiple chunks arriving in a short period in one StdWrittenEvent.
            // TODO groupWithin chops the first chunk (why?).
            //  We need an own implementation!
            //.stringToChar
            //.groupWithin(stdouterr.chunkSize, stdouterr.delay)
            //.map(_.convertToString)
            .foreach: chunk =>
              outErrStatistics(outerr).count(chunk.length):
                persistStdouterr(orderId, outerr, chunk)
            .compile
            .drain
            .recover: t =>
              IO(logger.error(s"$orderId $outerr: ${t.toStringWithCauses}")) *>
                persistStdouterr(orderId, outerr, s"\n\n--- ERROR: ${t.toStringWithCauses} ---")

        stdObservers
          .useInBackground:
            IO.both(
                writeStreamAsEvents(Stdout, stdObservers.outStream),
                writeStreamAsEvents(Stderr, stdObservers.errStream))
              .void
          .evalTap: _ =>
            startBarrier.await

  def detachProcessedOrder(orderId: OrderId): IO[Checked[Unit]] =
    orderToProcessing.remove(orderId)
      .flatMap(_.fold(IO.unit)(_.acknowldeged.complete(())))
      .as(Checked.unit)

  private val stdoutCommitDelay = CommitOptions(delay = subagentConf.stdoutCommitDelay)

  private def persistStdouterr(orderId: OrderId, t: StdoutOrStderr, chunk: String): IO[Unit] =
    journal
      .persistKeyedEventsLater((orderId <-: OrderStdWritten(t)(chunk)) :: Nil, stdoutCommitDelay)
      .map:
        case Left(problem) => logger.error(s"Emission of OrderStdWritten event failed: $problem")
        case Right(_) =>

  // Create the JobDriver if needed
  private def jobDriver(workflowPosition: WorkflowPosition)
  : IO[Checked[(WorkflowJob, JobDriver)]] =
    journal.state
      .map(state =>
        for
          workflow <- state.idToWorkflow.checked(workflowPosition.workflowId)
          jobKey <- workflow.positionToJobKey(workflowPosition.position)
          workflowJob <- workflow.keyToJob.checked(jobKey)
        yield
          jobKeyToJobDriver
            .getOrElseUpdate(jobKey,
              IO {
                val jobConf = JobConf(
                  jobKey, workflowJob, workflow, controllerId,
                  sigkillDelay = workflowJob.sigkillDelay
                    .getOrElse(subagentConf.defaultJobSigkillDelay),
                  jobLauncherConf.systemEncoding)
                new JobDriver(
                  jobConf,
                  id => journal.unsafeCurrentState().pathToJobResource.checked(id)/*live!*/,
                  JobLauncher.checked(jobConf, jobLauncherConf),
                  fileValueState)
              })
            .map(workflowJob -> _))
      .flatMap(_.sequence)

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit] =
    for
      maybeJobDriver <- IO(orderIdToJobDriver.get(orderId))
      _ <- maybeJobDriver
        .fold(IO(logger.debug(s"⚠️ killOrder $orderId => no JobDriver for Order")))(_
          .killOrder(orderId, signal))
    yield ()

  override def toString =
    s"DedicatedSubagent($subagentId $agentPath $controllerId)"


object DedicatedSubagent:
  private val logger = Logger[this.type]

  def resource(
    subagentId: SubagentId,
    subagentRunId: SubagentRunId,
    commandExecutor: SubagentCommandExecutor,
    journal: Journal[SubagentState],
    agentPath: AgentPath,
    controllerId: ControllerId,
    jobLauncherConf: JobLauncherConf,
    subagentConf: SubagentConf)
    (using ioRuntime: IORuntime)
  : Resource[IO, DedicatedSubagent] =
  Service.resource(IO(
    new DedicatedSubagent(
      subagentId, subagentRunId, commandExecutor, journal, agentPath, controllerId,
      jobLauncherConf, subagentConf)))

  private final class Processing(
    val workflowPosition: WorkflowPosition /*for check only*/ ,
    val fiber: FiberIO[OrderProcessed]):
    val acknowldeged = Deferred.unsafe[IO, Unit]
