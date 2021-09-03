package js7.agent.scheduler.job

import cats.syntax.traverse._
import java.util.Objects.requireNonNull
import js7.agent.scheduler.job.JobDriver._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.{RichCheckedTask, RichMonixTask}
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.monixutils.{AsyncMap, MonixDeadline}
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.implicits.RichIterableOnce
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobConf, JobResource, JobResourcePath}
import js7.data.order.Outcome.Succeeded
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.expression.Expression
import js7.executor.configuration.JobExecutorConf
import js7.executor.internal.JobExecutor
import js7.executor.{OrderProcess, ProcessOrder, StdObservers}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.cancelables.SerialCancelable
import scala.concurrent.Promise

private[agent] final class JobDriver(
  jobConf: JobConf,
  jobExecutorConf: JobExecutorConf,
  pathToJobResource: JobResourcePath => Checked[JobResource])
  (implicit scheduler: Scheduler, iox: IOExecutor)
{
  import jobConf.{jobKey, sigkillDelay, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.name)
  private val orderToProcess = AsyncMap.empty[OrderId, Entry]
  @volatile private var lastProcessTerminated: Promise[Unit] = null

  private val checkedJobExecutor: Checked[JobExecutor] =
    JobExecutor.checked(jobConf, jobExecutorConf, pathToJobResource)
      .map { jobExecutor =>
        jobExecutor.precheckAndWarn.runAsyncAndForget
        jobExecutor
      }

  for (problem <- checkedJobExecutor.left) logger.error(problem.toString)

  def stop(signal: ProcessSignal): Task[Unit] =
    Task.defer {
      logger.debug("Stop")
      lastProcessTerminated = Promise()
      orderToProcess.isEmpty
        .flatMap(isEmpty =>
          if (isEmpty)
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
                Task.fromFuture(lastProcessTerminated.future)
                  .logWhenItTakesLonger(s"Job '$jobKey' OrderProcess")))
        .flatMap(_ =>
          checkedJobExecutor.toOption.fold(Task.unit) { jobExecutor =>
            logger.trace("JobExecutor stop")
            jobExecutor
              .stop
              .logWhenItTakesLonger("Stop")
              .onErrorHandle(throwable =>
                logger.error("Stop", throwable.nullIfNoStackTrace))
          })
    }

  def processOrder(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression],
    stdObservers: StdObservers)
  : Task[Outcome] =
    Task.defer {
      val entry = new Entry(order.id)
      Task.pure(checkedJobExecutor)
        .flatMapT(jobExecutor => orderToProcess.insert(order.id, entry)
          // Read JobResources each time because they may change at any time
          .flatMapT(_ => Task.pure(jobConf.jobResourcePaths.traverse(pathToJobResource)))
          .map(_.map(jobResources => ProcessOrder(
            order, workflow, jobKey, jobResources,
            workflowJob.defaultArguments ++ defaultArguments,
            jobConf.controllerId, stdObservers)))
          .flatMapT(processOrder =>
            jobExecutor.startIfNeeded
              .flatMapT(_ => jobExecutor.prepareOrderProcess(processOrder))
              .flatMapT { orderProcess =>
                entry.orderProcess = Some(orderProcess)
                // Start the orderProcess. The future completes the stdObservers (stdout, stderr)
                orderProcess.start(processOrder.stdObservers)
                  .flatMap { runningProcessFuture =>
                    entry.runningSince = scheduler.now
                    scheduleTimeout(entry)
                    Task
                      .fromFuture(runningProcessFuture)
                      .map(entry.modifyOutcome)
                      .map {
                        case outcome: Succeeded =>
                          readErrorLine(processOrder).getOrElse(outcome)
                        case outcome => outcome
                      }
                  }
                  .tapEval(_ =>
                    entry.killSignal.traverse(signal =>
                      // killOrder may be called after processOrder
                      // but before process has been started, so we kill now.
                      killOrder(entry, signal)))
                  .onErrorHandle { t =>
                    logger.error(s"${order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                    Outcome.Failed.fromThrowable(t)
                  }
                  .map(Right(_))
              }))
      .materializeIntoChecked
      .map {
        case Left(problem) => Outcome.Disrupted(problem)
        case Right(outcome) => outcome
      }
      .guarantee(removeEntry(entry))
    }

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
        .flatMap(_ => orderToProcess.isEmpty)
        .map(isEmpty =>
          if (isEmpty && lastProcessTerminated != null) {
            lastProcessTerminated.trySuccess(())
          })
    }

  private def killOrderAndForget(entry: Entry, signal: ProcessSignal): Unit =
    killOrder(entry, signal)
      .onErrorHandle(t =>
        logger.error(t.toStringWithCauses + " - " + entry.orderId, t))
      .runAsyncAndForget

  def killOrder(orderId: OrderId, signal: ProcessSignal): Task[Unit] =
    orderToProcess
      .get(orderId)
      .flatMap(_.fold(Task.unit)(
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
    orderToProcess.all
      .map(_.values)
      .flatMap { entries =>
        if (entries.nonEmpty) logger.warn(
          s"Terminating, sending $signal to all $orderProcessCount tasks")
        entries.toVector.traverse(killProcess(_, signal))
      }
      .map(_.fold_)
      .onErrorHandle(t =>
        logger.error(t.toStringWithCauses, t))

  private def killProcess(entry: Entry, signal: ProcessSignal): Task[Unit] =
    Task.defer {
      if (signal == SIGKILL && entry.sigkilled)
        Task.unit
      else
        entry.orderProcess.fold(Task.unit) { orderProcess =>
          logger.info(s"Kill $signal ${entry.orderId}")
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

private[agent] object JobDriver
{
  private final class Entry(val orderId: OrderId) {
    var orderProcess: Option[OrderProcess] = None
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
