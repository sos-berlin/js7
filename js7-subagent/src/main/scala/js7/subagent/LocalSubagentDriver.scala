package js7.subagent

import cats.syntax.traverse._
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.{ProcessSignal, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax._
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerId
import js7.data.job.{JobConf, JobKey}
import js7.data.order.{Order, OrderId, OrderMark, Outcome}
import js7.data.state.AgentStateView
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.internal.JobLauncher
import js7.subagent.LocalSubagentDriver._
import js7.subagent.client.SubagentDriver
import js7.subagent.job.JobDriver
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration

final class LocalSubagentDriver(
  val subagentId: SubagentId,
  currentState: () => AgentStateView,
  onStdouterr: (OrderId, StdoutOrStderr, String) => Task[Unit],
  controllerId: ControllerId,
  jobLauncherConf: JobLauncherConf,
  protected val conf: SubagentDriver.Conf)
  (implicit scheduler: Scheduler)
extends SubagentDriver
{
  private val keyToJobDriver = AsyncMap.empty[JobKey, JobDriver]
  private val orderIdToJobDriver =
    new AsyncMap(Map.empty[OrderId, JobDriver]) with AsyncMap.WhenEmpty

  @volatile private var stopping = false  // TODO Reject further starts

  def start = Task {
    logger.debug("Start LocalSubagentDriver")
  }

  def stop(signal: Option[ProcessSignal]) =
    Task.defer {
      signal.fold(Task.unit)(killall) >>
        orderIdToJobDriver.whenEmpty
    }

  private def killall(signal: ProcessSignal): Task[Unit] =
    Observable
      .fromIterable(keyToJobDriver.toMap.values)
      .mapParallelUnordered(sys.runtime.availableProcessors)(jobDriver =>
        jobDriver
          .stop(signal)
          // TODO SIGKILL after SIGTERM and delay?
          .onErrorHandle(t => Task(logger.error(s"Stop $jobDriver: ${t.toStringWithCauses}"))))
      .completedL

  def processOrder(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  : Task[Outcome] =
    Task.defer {
      if (stopping)
        stoppingOutcome
      else
        orderToJobDriver(order)
          .flatMap {
            case Left(problem) => Task.pure(Outcome.Disrupted(problem))
            case Right((workflowJob, jobDriver)) =>
              withStdObservers(order.id, keepLastErrLine = workflowJob.failOnErrWritten, onStdouterr)(
                stdObservers =>
                  orderIdToJobDriver.update(order.id, _ => Task.pure(jobDriver)) >>
                    (if (stopping) // Check again in case of a race condition
                      stoppingOutcome
                    else
                      jobDriver
                        .processOrder(order, defaultArguments, stdObservers)
                        .guarantee(
                          orderIdToJobDriver.remove(order.id).void)))
          }
    }

  // Create the JobDriver if needed
  private def orderToJobDriver(order: Order[Order.Processing])
  : Task[Checked[(WorkflowJob, JobDriver)]] =
    Task {
      for {
        workflow <- currentState().idToWorkflow.checked(order.workflowId)
        jobKey <- workflow.positionToJobKey(order.position)
        workflowJob <- workflow.keyToJob.checked(jobKey)
        jobConf = JobConf(
          jobKey, workflowJob, workflow, controllerId,
          sigkillDelay = workflowJob.sigkillDelay getOrElse conf.defaultJobSigkillDelay)
      } yield
        keyToJobDriver
          .getOrElseUpdate(jobKey, Task(
            new JobDriver(
              jobConf,
              id => currentState().pathToJobResource.checked(id),
              JobLauncher.checked(
                jobConf,
                jobLauncherConf,
                id => currentState().pathToJobResource.checked(id)))))
          .map(workflowJob -> _)
    }.flatMap(_.sequence)

  def killProcess(orderId: OrderId, signal: ProcessSignal) =
    orderIdToJobDriver
      .get(orderId)
      .fold(Task.unit)(_
        .killOrder(orderId, signal))

  // TODO Call this!
  private def maybeKillOrder(order: Order[Order.Processing]): Unit =
    order.mark match {
      case Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(kill)))) =>
        maybeKillOrder(order, kill)

      case Some(OrderMark.Suspending(SuspensionMode(Some(kill)))) =>
        maybeKillOrder(order, kill)

      case _ =>
    }

  private def maybeKillOrder(order: Order[Order.Processing], kill: CancellationMode.Kill)
  : Task[Checked[Unit]] =
    if (kill.workflowPosition.forall(_ == order.workflowPosition))
      orderToJobDriver(order)
        .map(_.map(_._2
          .killOrder(
            order.id,
            if (kill.immediately) SIGKILL else SIGTERM)))
    else
      Task.pure(Checked.unit)

  override def toString =
    s"LocalSubagentDriver($subagentId)"
}

object LocalSubagentDriver
{
  private val logger = Logger(getClass)

  // TODO Use special Problem which is catched by the Director for trying another Subagent
  private val stoppingOutcome =
    Task.pure(Outcome.Disrupted(Problem.pure("Subagent is shutting down")))


  final case class StdouterrConf(chunkSize: Int, delay: FiniteDuration)
  object StdouterrConf {
    def apply(config: Config): StdouterrConf = new StdouterrConf(
      chunkSize = config.memorySizeAsInt("js7.order.stdout-stderr.chunk-size").orThrow,
      delay = config.getDuration("js7.order.stdout-stderr.delay").toFiniteDuration)
  }
}
