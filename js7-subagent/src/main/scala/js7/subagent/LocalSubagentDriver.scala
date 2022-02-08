package js7.subagent

import cats.syntax.parallel._
import cats.syntax.traverse._
import java.nio.file.Path
import js7.base.io.process.{ProcessSignal, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterableOnce
import js7.base.utils.ScalaUtils.chunkStrings
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.JournaledState
import js7.data.job.{JobConf, JobKey}
import js7.data.order.Outcome.Disrupted.JobSchedulerRestarted
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.state.AgentStateView
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.StatePersistence
import js7.launcher.StdObservers
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.subagent.LocalSubagentDriver._
import js7.subagent.client.SubagentDriver
import js7.subagent.job.JobDriver
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.Promise

final class LocalSubagentDriver[S0 <: AgentStateView with JournaledState[S0]](
  val subagentId: SubagentId,
  protected val persistence: StatePersistence[S0],
  val agentPath: AgentPath,
  val controllerId: ControllerId,
  jobLauncherConf: JobLauncherConf,
  protected val conf: SubagentDriver.Conf,
  valueDirectory: Path)
extends SubagentDriver
{
  protected type S = S0

  private val fileValueState = new FileValueState(valueDirectory)
  private val jobKeyToJobDriver = AsyncMap.empty[JobKey, JobDriver]
  private val orderIdToJobDriver =
    new AsyncMap(Map.empty[OrderId, JobDriver]) with AsyncMap.Stoppable

  val isHeartbeating = true

  def start = Task(logger.debug("Start LocalSubagentDriver"))

  def stop(signal: Option[ProcessSignal]) =
    Task.defer {
      val orderCount = orderIdToJobDriver.toMap.size
      logger.info(s"Stopping" + ((orderCount > 0) ?? s", waiting for $orderCount processes"))
      Task
        .parZip2(
          orderIdToJobDriver.stop
            .tapEval(_ => Task(logger.debug("orderIdToJobDriver stopped"))),
          signal.fold(Task.unit)(killAllAndStop)
            .tapEval(_ => Task(logger.debug("killAllAndStop completed"))))
        .tapEval(_ => Task(logger.debug("Stopped")))
        .*>(Task {
          fileValueState.close()
        })
    }

  private def killAllAndStop(signal: ProcessSignal): Task[Unit] =
    Task(jobKeyToJobDriver.toMap.values)
      .flatMap(_
        .toVector
        .parUnorderedTraverse(jobDriver => jobDriver
          .stop(signal)
          .onErrorHandle(t => logger.error(s"Stop $jobDriver: ${t.toStringWithCauses}")))
        .map(_.fold_))

  def processOrder(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Checked[Outcome]] =
    processOrder2(order, defaultArguments)
      .map(Right(_))
      //.flatMap { outcome =>
      //  val orderProcessed = OrderProcessed(outcome)
      //  persistence
      //    .persistKeyedEvent(order.id <-: orderProcessed)
      //    .rightAs(orderProcessed)
      //}

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
              .bracket(
                use = _.processOrder(order, defaultArguments, stdObservers))(
                release = _ => orderIdToJobDriver.remove(order.id).void))
    }

  private def observeStdoutAndStderr[A](orderId: OrderId, keepLastErrLine: Boolean)
    (body: StdObservers => Task[A])
  : Task[A] =
    Task.defer {
      val outErrStatistics = Map(
        Stdout -> new OutErrStatistics,
        Stderr -> new OutErrStatistics)

      val observingStarted = Map(
        Stdout -> Promise[Unit](),
        Stderr -> Promise[Unit]())

      val out, err = PublishSubject[String]()
      val stdObservers = new StdObservers(out, err, charBufferSize = conf.charBufferSize,
        keepLastErrLine = keepLastErrLine)

      def writeObservableAsEvents(outerr: StdoutOrStderr, observable: Observable[String]) =
        observable
          .doAfterSubscribe(Task(observingStarted(outerr).success(())))
          .buffer(Some(conf.stdouterr.delay), conf.stdouterr.chunkSize, toWeight = _.length)
          .flatMap(strings => Observable.fromIterable(chunkStrings(strings, conf.stdouterr.chunkSize)))
          .flatMap(chunk => Observable.fromTask(
            outErrStatistics(outerr).count(
              chunk.length,
              persistStdouterr(orderId, outerr, chunk))))
          .completedL

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

  // Create the JobDriver if needed
  private def jobDriver(workflowPosition: WorkflowPosition)
  : Task[Checked[(WorkflowJob, JobDriver)]] =
    persistence.state
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
                  sigkillDelay = workflowJob.sigkillDelay getOrElse conf.defaultJobSigkillDelay)
                new JobDriver(
                  jobConf,
                  id => persistence.currentState.pathToJobResource.checked(id)/*live!*/,
                  JobLauncher.checked(jobConf, jobLauncherConf),
                  fileValueState)
              }))
            .map(workflowJob -> _))
      .flatMap(_.sequence)

  def continueProcessingOrder(order: Order[Order.Processing]) =
    Task.pure(Right(Outcome.Disrupted(JobSchedulerRestarted)))

//  def continueProcessingOrder(order: Order[Order.Processing]) = {
//    val processed = OrderProcessed(Outcome.Disrupted(JobSchedulerRestarted))
//    persistence
//      .persistKeyedEvent(order.id <-: processed)
//      .rightAs(processed)
//  }

  def killProcess(orderId: OrderId, signal: ProcessSignal) =
    for {
      maybeJobDriver <- Task(orderIdToJobDriver.get(orderId))
      _ <- maybeJobDriver.fold(Task.unit)(_.killOrder(orderId, signal))
    } yield ()

  override def toString =
    s"LocalSubagentDriver(${subagentId.string})"
}

object LocalSubagentDriver
{
  private val logger = Logger(getClass)

  // TODO Use special Problem which is catched by the Director for trying another Subagent
  private val stoppingOutcome =
    Task.pure(Outcome.Disrupted(Problem.pure("Subagent is shutting down")))
}
