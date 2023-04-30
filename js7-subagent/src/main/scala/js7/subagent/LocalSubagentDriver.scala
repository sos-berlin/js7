package js7.subagent

import cats.effect.Resource
import cats.syntax.all.*
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.io.process.{ProcessSignal, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.{Checked, ProblemException}
import js7.base.service.Service
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.chunkStrings
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{JobConf, JobKey}
import js7.data.order.OrderEvent.{OrderProcessed, OrderStdWritten}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.Problems.SubagentShutDownBeforeProcessStartProblem
import js7.data.subagent.{SubagentDriverState, SubagentId}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.journal.CommitOptions
import js7.journal.state.Journal
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.subagent.LocalSubagentDriver.*
import js7.subagent.configuration.SubagentConf
import monix.eval.{Fiber, Task}
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.Promise

final class LocalSubagentDriver[S0 <: SubagentDriverState[S0]] private(
  val subagentId: SubagentId,
  protected val journal: Journal[S0],
  val agentPath: AgentPath,
  val controllerId: ControllerId,
  jobLauncherConf: JobLauncherConf,
  protected val conf: SubagentDriver.Conf,
  subagentConf: SubagentConf)
extends Service.StoppableByRequest with SubagentDriver
{
  protected type S = S0

  private val fileValueState = new FileValueState(subagentConf.valueDirectory)
  private val jobKeyToJobDriver = AsyncMap.empty[JobKey, JobDriver]
  private val orderIdToJobDriver = AsyncMap.stoppable[OrderId, JobDriver]()
  private val stoppingLock = AsyncLock()
  @volatile private var _testFailover = false

  protected def isShuttingDown = false

  def start =
    startService(
      untilStopRequested *>
        terminate(Some(SIGTERM)))

  def terminate(signal: Option[ProcessSignal]) =
    stoppingLock.lock(Task.defer {
      val orderCount = orderIdToJobDriver.toMap.size
      if (orderCount > 0) {
        logger.info(s"Stopping, waiting for $orderCount processes")
      }
      Task
        .parZip2(
          orderIdToJobDriver.stop,
          signal.fold(Task.unit)(killAllAndStop))
        .*>(Task {
          fileValueState.close()
        })
    })

  private def killAllAndStop(signal: ProcessSignal): Task[Unit] =
    logger.debugTask("killAllAndStop", signal)(
      Task(jobKeyToJobDriver.toMap.values)
        .flatMap(_
          .toVector
          .parUnorderedTraverse(jobDriver => jobDriver
            .stop(signal)
            .onErrorHandle(t => logger.error(s"Stop $jobDriver: ${t.toStringWithCauses}")))
          .map(_.combineAll)))

  def tryShutdown =
    Task.unit

  def startOrderProcessing(order: Order[Order.Processing]): Task[Checked[Fiber[OrderProcessed]]] =
    orderToExecuteDefaultArguments(order)
      .flatMapT(defaultArguments =>
        processOrder2(order, defaultArguments)
          .map(OrderProcessed(_))
          .flatTap(orderProcessed =>
            if (_testFailover && orderProcessed.outcome.isInstanceOf[Outcome.Killed])
              Task(logger.warn(
                s"Suppressed due to failover by command: ${order.id} <-: $orderProcessed"))
            else
              journal.persistKeyedEvent(order.id <-: orderProcessed)
                .orThrow)
          .start
          .map(Right(_)))

  def processOrder2(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Outcome] =
    jobDriver(order.workflowPosition).flatMap {
      case Left(problem) =>
        Task.pure(Outcome.Disrupted(problem))

      case Right((workflowJob, jobDriver)) =>
        observeStdoutAndStderr(order.id, keepLastErrLine = workflowJob.failOnErrWritten)(
          stdObservers =>
            orderIdToJobDriver
              .put(order.id, jobDriver)
              .map(Right(_))
              .onErrorRecover { case ProblemException(problem)
                if orderIdToJobDriver.isStoppingWith(problem) =>
                Left(Outcome.processLost(SubagentShutDownBeforeProcessStartProblem))
              }
              .bracket(
                use = {
                  case Left(processLost) => Task.pure(processLost)
                  case Right(jobDriver) =>
                    jobDriver.processOrder(order, defaultArguments, stdObservers)
                })(
                release = _ => orderIdToJobDriver.remove(order.id).void))
    }

  private def observeStdoutAndStderr[A](orderId: OrderId, keepLastErrLine: Boolean)
    (body: StdObservers => Task[A])
  : Task[A] =
    Task.defer {
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
        result <- body(stdObservers)
        _ <- stdObservers.stop /*may already have been stopped by OrderProcess/JobDriver*/
        _ <- observingOutErr.join
      } yield {
        if (outErrStatistics(Stdout).isRelevant || outErrStatistics(Stderr).isRelevant) {
          logger.debug(s"stdout: ${outErrStatistics(Stdout)}, stderr: ${outErrStatistics(Stderr)}")
        }
        result
      }
    }

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

  def recoverOrderProcessing(order: Order[Order.Processing]) =
    emitOrderProcessLost(order)
      .map(_.orThrow)
      .start
      .map(Right(_))

  def killProcess(orderId: OrderId, signal: ProcessSignal) =
    for {
      maybeJobDriver <- Task(orderIdToJobDriver.get(orderId))
      _ <- maybeJobDriver.fold(Task.unit)(_.killOrder(orderId, signal))
    } yield ()

  def testFailover(): Unit =
    _testFailover = true

  override def toString =
    s"LocalSubagentDriver(${subagentId.string})"
}

object LocalSubagentDriver
{
  private val logger = Logger(getClass)

  def resource[S <: SubagentDriverState[S]](
    subagentId: SubagentId,
    journal: Journal[S],
    agentPath: AgentPath,
    controllerId: ControllerId,
    jobLauncherConf: JobLauncherConf,
    conf: SubagentDriver.Conf,
    subagentConf: SubagentConf)
  : Resource[Task, LocalSubagentDriver[S]] =
  Service.resource(Task(
    new LocalSubagentDriver[S](
      subagentId, journal, agentPath, controllerId, jobLauncherConf, conf, subagentConf)))
}
