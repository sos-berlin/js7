package js7.subagent

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import cats.syntax.all.*
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.io.process.{ProcessSignal, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.service.Service
import js7.base.utils.AsyncLock
import js7.base.utils.CatsUtils.completedFiber
import js7.base.utils.ScalaUtils.chunkStrings
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{JobConf, JobKey}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.Problems.{SubagentIdMismatchProblem, SubagentIsShuttingDownProblem, SubagentRunIdMismatchProblem, SubagentShutDownBeforeProcessStartProblem}
import js7.data.subagent.SubagentCommand.CoupleDirector
import js7.data.subagent.{SubagentId, SubagentRunId, SubagentState}
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
import monix.eval.{Fiber, Task}
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.Promise

private final class DedicatedSubagent private(
  val subagentId: SubagentId,
  val subagentRunId: SubagentRunId,
  protected val journal: Journal[SubagentState],
  val agentPath: AgentPath,
  val controllerId: ControllerId,
  jobLauncherConf: JobLauncherConf,
  subagentConf: SubagentConf)
extends Service.StoppableByRequest
{
  protected type S = SubagentState

  private val fileValueState = new FileValueState(subagentConf.valueDirectory)
  private val jobKeyToJobDriver = AsyncMap.empty[JobKey, JobDriver]
  private val orderIdToJobDriver = AsyncMap.stoppable[OrderId, JobDriver]()
  private val stoppingLock = AsyncLock()
  private val orderToProcessing = AsyncMap.stoppable[OrderId, Processing]()
  @volatile private var _dontWaitForDirector = false
  private val shuttingDown = Atomic(false)

  def isLocal = true

  def isShuttingDown: Boolean =
    shuttingDown()

  protected def start =
    startService(
      untilStopRequested *>
        terminate(Some(SIGTERM)))

  private[subagent] def terminate(
    signal: Option[ProcessSignal],
    dontWaitForDirector: Boolean = false)
  : Task[Unit] =
    stoppingLock.lock(Task.defer {
      _dontWaitForDirector |= dontWaitForDirector
      val first = !shuttingDown.getAndSet(true)
      Task
        .when(first)(Task.defer {
          val orderCount = orderIdToJobDriver.toMap.size
          if (orderCount > 0) {
            logger.info(s"Stopping, waiting for $orderCount processes")
          }
          Task
            .parZip2(
              orderIdToJobDriver.stop,
              signal.fold(Task.unit)(killAndStopAllJobs))
            .*>(Task {
              fileValueState.close()
            })
            .*>(orderToProcessing.initiateStopWithProblem(SubagentIsShuttingDownProblem))
            .*>(Task.defer {
              if (dontWaitForDirector) Task {
                for (orderId <- orderToProcessing.toMap.keys.toVector.sorted) logger.warn(
                  s"Shutdown: Agent Director has not yet acknowledged processing of $orderId")
              } else
                awaitOrderAcknowledgements *>
                  // Await process termination and DetachProcessedOrder commands
                  orderToProcessing.whenStopped
                    .logWhenItTakesLonger("Director-acknowledged Order processes")
            })
        })
    })

  private[subagent] def executeCoupleDirector(cmd: CoupleDirector): Task[Checked[Unit]] =
    Task {
      for {
        _ <- checkSubagentId(cmd.subagentId)
        _ <- checkSubagentRunId(cmd.subagentRunId)
        _ <- journal.eventWatch.checkEventId(cmd.eventId)
      } yield ()
    }

  private def checkSubagentId(requestedSubagentId: SubagentId): Checked[Unit] =
    (requestedSubagentId == subagentId) !!
      SubagentIdMismatchProblem(requestedSubagentId, subagentId)

  private[subagent] def checkSubagentRunId(requestedSubagentRunId: SubagentRunId): Checked[Unit] =
    if (requestedSubagentRunId != subagentRunId) {
      val problem = SubagentRunIdMismatchProblem(subagentId)
      logger.warn(
        s"$problem, requestedSubagentRunId=$requestedSubagentRunId, " +
          s"agentRunId=${this.subagentRunId}")
      Left(problem)
    } else
      Checked.unit

  private def awaitOrderAcknowledgements: Task[Unit] =
    Task.defer {
      val oToP = orderToProcessing.toMap.toVector
      for (orderId <- oToP.map(_._1).sorted) logger.info(
        s"🟡 Delaying shutdown until Agent Director has acknowledged processing of $orderId")
      oToP
        .parTraverse { case (orderId, processing) =>
          processing.acknowldeged.get
            .flatMap(_ =>
              Task(logger.info(s"🟢 Director has acknowledged processing of $orderId")))
        }
        .map(_.combineAll)
    }

  private def killAndStopAllJobs(signal: ProcessSignal): Task[Unit] =
    logger.debugTask("killAndStopAllJobs", signal)(
      Task(jobKeyToJobDriver.toMap.values)
        .flatMap(_
          .toVector
          .parUnorderedTraverse(jobDriver => jobDriver
            .stop(signal)
            .onErrorHandle(t => logger.error(s"Stop $jobDriver: ${t.toStringWithCauses}")))
          .map(_.combineAll)))

  def startOrderProcess(order: Order[Order.Processing], defaultArguments: Map[String, Expression])
  : Task[Checked[Fiber[OrderProcessed]]] =
    Task.defer {
      orderToProcessing
        .updateChecked(order.id, {
          case Some(existing) =>
            Task.pure(
              if (existing.workflowPosition != order.workflowPosition) {
                val problem = Problem.pure(
                  "Duplicate SubagentCommand.StartOrder with different Order position")
                logger.warn(s"$problem:")
                logger.warn(s"  Added order   : ${order.id} ${order.workflowPosition}")
                logger.warn(s"  Existing order: ${order.id} ${existing.workflowPosition}")
                Left(problem)
              } else
                Right(existing)) // Idempotency: Order process has already been started

          case None =>
            startOrderProcess2(order, defaultArguments)
              .guaranteeCase {
                case ExitCase.Completed =>
                  Task.unless(!_dontWaitForDirector) {
                    logger.warn(
                      s"dontWaitForDirector: ${order.id} <-: OrderProcessed event may get lost")
                    orderToProcessing.remove(order.id).void
                  }

                case _ => orderToProcessing.remove(order.id).void // Tidy-up on failure
              }
              .map(fiber => Right(new Processing(order.workflowPosition, fiber)))
        })
        .map(_.map(_.fiber))
    }

  private def startOrderProcess2(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Fiber[OrderProcessed]] =
    startOrderProcess3(order, defaultArguments)
      .flatMap(_
        .join
        .onErrorHandle(Outcome.Failed.fromThrowable)
        .flatMap(outcome =>
          journal
            .persistKeyedEvent(order.id <-: OrderProcessed(outcome))
            .map(_.orThrow._1.value.event))
        .start)

  def startOrderProcess3(order: Order[Order.Processing], defaultArguments: Map[String, Expression])
  : Task[Fiber[Outcome]] =
    jobDriver(order.workflowPosition).flatMap {
      case Left(problem) =>
        Task.pure(completedFiber(Outcome.Disrupted(problem)))

      case Right((workflowJob, jobDriver)) =>
        val releaseAssignment = orderIdToJobDriver.remove(order.id).void
        orderIdToJobDriver
          .put(order.id, jobDriver)
          .*>(
            stdObserversResource(order.id, keepLastErrLine = workflowJob.failOnErrWritten)
              .allocated)
          .flatMap { case (stdObservers, releaseStdObservers) =>
            jobDriver.startOrderProcess(order, defaultArguments, stdObservers)
              .flatMap(_
                .join
                .guarantee(releaseStdObservers)
                .guarantee(releaseAssignment)
                .start
                .guaranteeExceptWhenCompleted(releaseStdObservers))
          }
          .guaranteeExceptWhenCompleted(releaseAssignment)
          .onErrorRecover {
            case ProblemException(problem) if orderIdToJobDriver.isStoppingWith(problem) =>
              completedFiber(Outcome.processLost(SubagentShutDownBeforeProcessStartProblem))
          }
    }

  private def stdObserversResource(orderId: OrderId, keepLastErrLine: Boolean)
  : Resource[Task, StdObservers] =
    Resource
      .make(
        acquire = Task.defer {
          import subagentConf.{outerrCharBufferSize, stdouterr}

          val outErrStatistics = Map[StdoutOrStderr, OutErrStatistics](
            Stdout -> new OutErrStatistics,
            Stderr -> new OutErrStatistics)

          val observingStarted = Map[StdoutOrStderr, Promise[Unit]](
            Stdout -> Promise[Unit](),
            Stderr -> Promise[Unit]())

          val out, err = PublishSubject[String]()
          val stdObservers = new StdObservers(out, err, charBufferSize = outerrCharBufferSize,
            keepLastErrLine = keepLastErrLine)

          def writeObservableAsEvents(outerr: StdoutOrStderr, observable: Observable[String]) =
            Task.defer {
              val correlId = CorrelId.current
              observable
                .doAfterSubscribe(Task(observingStarted(outerr).success(())))
                .buffer(Some(stdouterr.delay), stdouterr.chunkSize, toWeight = _.length)
                .flatMap(strings => Observable.fromIterable(chunkStrings(strings, stdouterr.chunkSize)))
                .flatMap(chunk => Observable.fromTask(
                  outErrStatistics(outerr).count(
                    chunk.length,
                    correlId.bind(
                      persistStdouterr(orderId, outerr, chunk)))))
                .completedL
            }

          val observeOutErr = Task
            .parZip2(
              writeObservableAsEvents(Stdout, out),
              writeObservableAsEvents(Stderr, err))
            .void

          for {
            observingOutErr <- observeOutErr.start
            _ <- observingStarted.values.toSeq.traverse(promise => Task.fromFuture(promise.future))
          } yield (outErrStatistics, stdObservers, observingOutErr)
        })(
        release = {
          case (outErrStatistics, stdObservers, observingOutErr) =>
            for {
              _ <- stdObservers.stop /*may already have been stopped by OrderProcess/JobDriver*/
              _ <- observingOutErr.join
            } yield {
              if (outErrStatistics(Stdout).isRelevant || outErrStatistics(Stderr).isRelevant) {
                logger.debug(s"stdout: ${outErrStatistics(Stdout)}, stderr: ${outErrStatistics(Stderr)}")
              }
            }
        })
      .map(_._2)

  def detachProcessedOrder(orderId: OrderId): Task[Checked[Unit]] =
    orderToProcessing.remove(orderId)
      .flatMap(_.fold(Task.unit)(_.acknowldeged.complete(())))
      .as(Checked.unit)

  private val stdoutCommitDelay = CommitOptions(delay = subagentConf.stdoutCommitDelay)

  private def persistStdouterr(orderId: OrderId, t: StdoutOrStderr, chunk: String): Task[Unit] =
    journal
      .persistKeyedEventsLater((orderId <-: OrderStdWritten(t)(chunk)) :: Nil, stdoutCommitDelay)
      .map {
        case Left(problem) => logger.error(s"Emission of OrderStdWritten event failed: $problem")
        case Right(_) =>
      }

  // Create the JobDriver if needed
  private def jobDriver(workflowPosition: WorkflowPosition)
  : Task[Checked[(WorkflowJob, JobDriver)]] =
    journal.state
      .map(state =>
        for {
          workflow <- state.idToWorkflow.checked(workflowPosition.workflowId)
          jobKey <- workflow.positionToJobKey(workflowPosition.position)
          workflowJob <- workflow.keyToJob.checked(jobKey)
        } yield
          jobKeyToJobDriver
            .getOrElseUpdate(jobKey,
              Task.deferAction(implicit scheduler => Task {
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
              }))
            .map(workflowJob -> _))
      .flatMap(_.sequence)

  def killProcess(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    for {
      maybeJobDriver <- Task(orderIdToJobDriver.get(orderId))
      _ <- maybeJobDriver
        .fold(Task(logger.debug(s"⚠️ killOrder $orderId => no JobDriver for Order")))(_
          .killOrder(orderId, signal))
    } yield ()

  override def toString =
    s"DedicatedSubagent($subagentId $agentPath $controllerId)"
}

private object DedicatedSubagent
{
  private val logger = Logger(getClass)

  def resource(
    subagentId: SubagentId,
    subagentRunId: SubagentRunId,
    journal: Journal[SubagentState],
    agentPath: AgentPath,
    controllerId: ControllerId,
    jobLauncherConf: JobLauncherConf,
    subagentConf: SubagentConf)
  : Resource[Task, DedicatedSubagent] =
  Service.resource(Task(
    new DedicatedSubagent(
      subagentId, subagentRunId, journal, agentPath, controllerId, jobLauncherConf, subagentConf)))

  private final class Processing(
    val workflowPosition: WorkflowPosition /*for check only*/ ,
    val fiber: Fiber[OrderProcessed])
  {
    val acknowldeged = Deferred.unsafe[Task, Unit]
  }
}
