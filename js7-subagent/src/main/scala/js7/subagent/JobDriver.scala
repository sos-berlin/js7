package js7.subagent

import cats.effect.Resource
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import java.util.Objects.requireNonNull
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.monixutils.{AsyncMap, MonixDeadline}
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.Outcome.Succeeded
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.expression.Expression
import js7.data.value.expression.scopes.FileValueState
import js7.launcher.internal.JobLauncher
import js7.launcher.{OrderProcess, ProcessOrder, StdObservers}
import js7.subagent.JobDriver.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.SerialCancelable
import scala.concurrent.Promise

final class JobDriver(
  jobConf: JobConf,
  pathToJobResource: JobResourcePath => Checked[JobResource],
  checkedJobLauncher: Checked[JobLauncher],
  fileValueState: FileValueState)
  (implicit scheduler: Scheduler)
{
  import jobConf.{jobKey, sigkillDelay, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.name)
  private val orderToProcess = AsyncMap.empty[OrderId, Entry]
  @volatile private var lastProcessTerminated: Promise[Unit] = null

  for (launcher <- checkedJobLauncher) {
    // TODO JobDriver.start(): Task[Checked[JobDriver]]
    launcher.precheckAndWarn.runAsyncAndForget
  }

  for (problem <- checkedJobLauncher.left) logger.error(problem.toString)

  def stop(signal: ProcessSignal): Task[Unit] =
    Task.defer {
      logger.debug("Stop")
      lastProcessTerminated = Promise()
      (if (orderToProcess.isEmpty)
        Task.unit
      else
        killAll(signal)
          .map(_ =>
            if (signal != SIGKILL) {
              scheduler.scheduleOnce(sigkillDelay) {
                killAll(SIGKILL).runAsyncAndForget
              }
            })
          .flatMap(_ =>
            Task.fromFuture(lastProcessTerminated.future))
          .logWhenItTakesLonger(s"'killing all $jobKey processes'")
      ).flatMap(_ =>
        checkedJobLauncher.toOption.fold(Task.unit) { jobLauncher =>
          logger.trace("JobLauncher stop")
          jobLauncher
            .stop
            .logWhenItTakesLonger
            .onErrorHandle(throwable =>
              logger.error(s"Stop '$jobLauncher' failed: ${throwable.toStringWithCauses}",
                throwable.nullIfNoStackTrace))
        })
    }

  def processOrder(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : Task[Outcome] =
    Task.defer {
      val entry = new Entry(order.id)
      Task.pure(checkedJobLauncher)
        .flatMapT(jobLauncher => orderToProcess.insert(order.id, entry)
          .flatMapT(_ => Task.pure(processOrderResource(order, defaultArguments, stdObservers)))
          .flatMapT(_.use(processOrder =>
            jobLauncher.startIfNeeded
              .flatMapT(_ => jobLauncher.toOrderProcess(processOrder))
              .flatMapT { orderProcess =>
                entry.orderProcess = Some(orderProcess)
                // Start the orderProcess. The future completes the stdObservers (stdout, stderr)
                orderProcess.start(processOrder.stdObservers)
                  .flatMap { runningProcess =>
                    val whenCompleted = runningProcess
                      .onErrorHandle { t =>
                        logger.error(s"${order.id} Job failed: ${t.toStringWithCauses}", t)
                        Outcome.Failed.fromThrowable(t)
                      }
                      .runToFuture
                    entry.terminated.completeWith(whenCompleted)
                    val maybeKillAfterStart = entry.killSignal.traverse(killOrder(entry, _))
                    val awaitTermination = Task.defer {
                      entry.runningSince = scheduler.now
                      scheduleTimeout(entry)
                      Task
                        .fromFuture(whenCompleted)
                        .map(entry.modifyOutcome)
                        .map {
                          case outcome: Succeeded =>
                            readErrorLine(processOrder).getOrElse(outcome)
                          case outcome => outcome
                        }
                    }
                    Task.parMap2(maybeKillAfterStart, awaitTermination)((_, outcome) => outcome)
                  }
                  .onErrorHandle { t =>
                    logger.error(s"${order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                    Outcome.Failed.fromThrowable(t)
                  }
                  .map(Right(_))
              })))
        .materializeIntoChecked
        .map {
          case Left(problem) => Outcome.Disrupted(problem)
          case Right(outcome) => outcome
        }
        .guarantee(removeEntry(entry))
    }

  private def processOrderResource(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : Checked[Resource[Task, ProcessOrder]] =
    checkedJobLauncher
      // Read JobResources each time because they may change at any time
      .flatMap(_ => jobConf.jobResourcePaths.traverse(pathToJobResource))
      .map(jobResources =>
        ProcessOrder.resource(
          order, workflow, jobKey, workflowJob, jobResources,
          workflowJob.defaultArguments ++ defaultArguments,
          jobConf.controllerId, stdObservers,
          fileValueState))

  private def scheduleTimeout(entry: Entry): Unit = {
    requireNonNull(entry.runningSince)
    for (t <- workflowJob.timeout) {
      entry.timeoutSchedule := scheduler.scheduleOnce(t) {
        entry.timedOut = true
        logger.warn("OrderProcess for " + entry.orderProcess + " has been timed out after " +
          entry.runningSince.elapsed.pretty + " and will be killed now")
        killOrderAndForget(entry, SIGTERM)
      }
    }
  }

  private def readErrorLine(processOrder: ProcessOrder): Option[Outcome.Failed] =
    processOrder.stdObservers.errorLine
      .map { errorLine =>
        assert(workflowJob.failOnErrWritten) // see OrderActor
        Outcome.Failed(Some(s"The job's error channel: $errorLine"))
      }

  private def removeEntry(entry: Entry): Task[Unit] =
    Task.defer {
      import entry.orderId
      entry.timeoutSchedule.cancel()
      orderToProcess
        .remove(orderId)
        .map(_ =>
          if (orderToProcess.isEmpty && lastProcessTerminated != null) {
            lastProcessTerminated.trySuccess(())
          })
    }

  private def killOrderAndForget(entry: Entry, signal: ProcessSignal): Unit =
    killOrder(entry, signal)
      .onErrorHandle(t =>
        logger.error(t.toStringWithCauses + " - " + entry.orderId, t))
      .runAsyncAndForget

  def killOrder(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    Task.defer(
      orderToProcess
        .get(orderId)
        .fold(Task.unit)(
          killOrder(_, signal)))

  private def killOrder(entry: Entry, signal_ : ProcessSignal): Task[Unit] = {
    val signal = if (sigkillDelay.isZeroOrBelow) SIGKILL else signal_
    entry.killSignal = Some(signal)
    killProcess(entry, signal)
      .map(_ =>
        if (signal != SIGKILL) {
          scheduler.scheduleOnce(sigkillDelay) {
            killProcess(entry, SIGKILL)
              .runAsyncAndForget
          }
        })
  }

  private def killAll(signal: ProcessSignal): Task[Unit] =
    Task.defer {
      val entries = orderToProcess.toMap.values
      if (entries.nonEmpty) logger.warn(
        s"Terminating, sending $signal to $orderProcessCount processes")
      entries
        .toVector
        .traverse(killProcess(_, signal))
        .map(_.combineAll)
        .onErrorHandle(t =>
          logger.error(t.toStringWithCauses, t))
    }

  private def killProcess(entry: Entry, signal: ProcessSignal): Task[Unit] =
    Task.defer {
      if (signal == SIGKILL && entry.sigkilled)
        Task.unit
      else
        entry.orderProcess match {
          case None =>
            logger.debug(s"killProcess(${entry.orderId},  $signal): no OrderProcess")
            Task.unit
          case Some(orderProcess) =>
            logger.debug(s"Kill $signal ${entry.orderId}")
            entry.isKilled = true
            entry.sigkilled |= signal == SIGKILL
            Task
              .defer/*catch inside task*/ {
                orderProcess.cancel(immediately = signal == SIGKILL)
              }
              .onErrorHandle(t => logger.error(
                s"Kill ${entry.orderId}}: ${t.toStringWithCauses}", t.nullIfNoStackTrace))
        }
    }

  override def toString = s"JobDriver($jobKey ${workflowJob.executable})"

  private def orderProcessCount = orderToProcess.size
}

object JobDriver
{
  private final class Entry(val orderId: OrderId) {
    var orderProcess: Option[OrderProcess] = None
    val terminated = Promise[Outcome.Completed]()
    var killSignal: Option[ProcessSignal] = None
    val timeoutSchedule = SerialCancelable()
    var runningSince: MonixDeadline = null
    var isKilled = false
    var sigkilled = false
    var timedOut = false

    def modifyOutcome(outcome: Outcome) =
      outcome match {
        case outcome: Outcome.Completed =>
          if (timedOut)
             Outcome.TimedOut(outcome)
           else if (isKilled)
             Outcome.Killed(outcome)
           else
             outcome
        case o => o
      }
  }
}
