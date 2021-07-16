package js7.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import cats.syntax.traverse._
import js7.agent.scheduler.job.JobActor._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.{RichDuration, RichFiniteDuration}
import js7.base.utils.Collections.implicits.InsertableMutableMap
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
import scala.collection.mutable
import scala.util.{Failure, Success}

private[agent] final class JobActor private(
  jobConf: JobConf,
  jobExecutorConf: JobExecutorConf,
  pathToJobResource: JobResourcePath => Checked[JobResource])
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends Actor with Stash
{
  import jobConf.{jobKey, sigkillDelay, timeout, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.name)
  private val orderToProcess = mutable.Map.empty[OrderId, Entry]
  private var waitingForNextOrder = false
  private var terminating = false

  private val checkedJobExecutor: Checked[JobExecutor] =
    JobExecutor.checked(jobConf, jobExecutorConf, pathToJobResource)
      .map { jobExecutor =>
        jobExecutor.precheckAndWarn.runAsyncAndForget
        jobExecutor
      }

  for (problem <- checkedJobExecutor.left) logger.error(problem.toString)

  override def postStop() = {
    killAll(SIGKILL)
    super.postStop()
  }

  def receive = {
    case Input.OrderAvailable =>
      handleIfReadyForOrder()

    case Input.ProcessOrder(order, defaultArguments, stdObservers) if waitingForNextOrder =>
      val sender = this.sender()
      waitingForNextOrder = false

      // Read JobResources each time because they may change at any time
      (for {
        jobExecutor <- checkedJobExecutor
        resourcesPaths <- jobConf.jobResourcePaths.traverse(pathToJobResource)
      } yield jobExecutor -> resourcesPaths)
      match {
        case Left(problem) =>
          replyWithOrderProcessed(order.id, Outcome.Disrupted(problem))

        case Right((jobExecutor, jobResources)) =>
          val processOrder = ProcessOrder(
            order, workflow, jobKey, jobResources,
            defaultArgumentExpressions = workflowJob.defaultArguments ++ defaultArguments,
            jobConf.controllerId, stdObservers)
          jobExecutor.start
            .materializeIntoChecked
            .map { checked =>
              self.!(Internal.Step(processOrder, jobExecutor, checked))(sender)
            }
            .runToFuture
      }

    case Internal.Step(processOrder, jobExecutor, checkedStart) =>
      import processOrder.order
      checkedStart match {
        case Left(problem) =>
          replyWithOrderProcessed(order.id, Outcome.Disrupted(problem))

        case Right(()) =>
          val sender = this.sender()
          jobExecutor
            .prepareOrderProcess(processOrder)
            .materializeIntoChecked
            .foreach(o => self.!(Internal.Process(processOrder, o))(sender))
      }
      handleIfReadyForOrder()

    case Internal.Process(processOrder: ProcessOrder, Left(problem)) =>
      import processOrder.order
      val sender = this.sender()
      logger.debug(s"Order '${order.id.string}' step could not be started: $problem")
      self.!(Internal.TaskFinished(order.id, Outcome.Disrupted(problem)))(sender)

    case Internal.Process(processOrder, Right(orderProcess)) =>
      import processOrder.order
      val sender = this.sender()

      val entry = new Entry(orderProcess)
      orderToProcess.insert(order.id -> entry)

      // Start the orderProcess. The future completes the out/err observers
      val future = orderProcess.runToFuture(processOrder.stdObservers)
      entry.runningSince = scheduler.now

      for (t <- timeout) {
        // FIXME Maybe too early: the future may not yet have started the process
        // Separate start: Future[?] and terminated: Future[Completed]
        entry.timeoutSchedule := scheduler.scheduleOnce(t) {
          self ! Internal.Timeout(order.id)
        }
      }

      future.onComplete { tried =>
        val outcome = tried match {
          case Success(outcome: Succeeded) =>
            processOrder.stdObservers.errorLine match {
              case None => outcome
              case Some(errorLine) =>
                assert(workflowJob.failOnErrWritten) // see OrderActor
                Outcome.Failed(Some(s"The job's error channel: $errorLine"))
            }
          case Success(o) => o
          case Failure(t) =>
            logger.error(s"${order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
            Outcome.Failed.fromThrowable(t)
        }
        self.!(Internal.TaskFinished(order.id, outcome))(sender)
      }

    case Internal.Timeout(orderId) =>
      for (entry <- orderToProcess.get(orderId)) {
        entry.timedOut = true
        logger.warn("OrderProcess for " + orderId + " has been timed out after " +
          entry.runningSince.elapsed.pretty + " and will be killed now")
        killOrder(orderId, Some(SIGTERM))
      }

    case Internal.TaskFinished(orderId, outcome) =>
      for (o <- orderToProcess.get(orderId)) o.timeoutSchedule.cancel()
      replyWithOrderProcessed(orderId, outcome)
      orderToProcess -= orderId

    case Input.Terminate(maybeSignal) =>
      logger.debug("Terminate")
      terminating = true
      for (signal <- maybeSignal) {
        killAll(signal)
      }
      if (!maybeSignal.contains(SIGKILL) && orderProcessCount > 0) {
        scheduler.scheduleOnce(sigkillDelay) {
          self ! Internal.KillAll
        }
      }
      continueTermination()

    case Input.KillProcess(orderId, maybeSignal) =>
      killOrder(orderId, maybeSignal)

    case Internal.KillAll =>
      killAll(SIGKILL)

    case Internal.KillOrder(orderId) =>
      kill(orderId, SIGKILL)
  }

  private def replyWithOrderProcessed(orderId: OrderId, outcome_ : Outcome): Unit = {
    val outcome = outcome_ match {
      case o: Outcome.Completed =>
        orderToProcess.get(orderId) match {
          case None => o
          case Some(entry) =>
            if (entry.timedOut)
              Outcome.TimedOut(o)
            else if (entry.isKilled)
              Outcome.Killed(o)
            else o
        }
      case o => o
    }
    sender() ! Response.OrderProcessed(orderId, outcome)
    continueTermination()
    handleIfReadyForOrder()
  }

  private def handleIfReadyForOrder(): Unit =
    if (!waitingForNextOrder && !terminating && orderProcessCount < workflowJob.parallelism) {
      context.parent ! Output.ReadyForOrder
      waitingForNextOrder = true
    }

  private def killAll(signal: ProcessSignal): Unit =
    if (orderToProcess.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $orderProcessCount tasks")
      for (orderId <- orderToProcess.keys.toVector/*copy*/) {
        kill(orderId, signal)
      }
    }

  private def killOrder(orderId: OrderId, maybeSignal: Option[ProcessSignal]): Unit = {
    val signal = maybeSignal match {
      case Some(signal) => Some(signal)
      case None => !sigkillDelay.isPositive ? SIGKILL  // SIGKILL immediately on sigkillDelay = 0
    }
    for (signal <- signal) {
      kill(orderId, signal)
    }
    if (!signal.contains(SIGKILL)) {
      if (orderToProcess.contains(orderId)) {
        scheduler.scheduleOnce(sigkillDelay) {
          self ! Internal.KillOrder(orderId)
        }
      }
    }
  }

  private def kill(orderId: OrderId, signal: ProcessSignal): Unit =
    for (entry <- orderToProcess.get(orderId)) {
      logger.info(s"Kill $signal $orderId")
      entry.isKilled = true

      Task
        .defer/*catch exception*/ {
          entry.orderProcess.cancel(immediately = signal == SIGKILL)
        }
        .onErrorHandle { t =>
          logger.error(s"Kill $orderId: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
        }
        .runAsyncAndForget
    }

  private def continueTermination(): Unit =
    if (terminating) {
      if (orderToProcess.isEmpty) {
        checkedJobExecutor.fold(_ => Task.unit, _.stop)
          .onErrorHandle { throwable =>
            logger.error(s"Stop: ${throwable.toStringWithCauses}", throwable.nullIfNoStackTrace)
          }
          .map(_ => context.stop(self))
          .runAsyncAndForget
      } else {
        logger.debug(s"Awaiting termination of ${orderToProcess.size} tasks")
      }
    }

  override def toString = s"JobActor(${jobKey.toString})"

  private def orderProcessCount = orderToProcess.size
}

private[agent] object JobActor
{
  def props(
    jobConf: JobConf,
    executorConf: JobExecutorConf,
    pathToJobResource: JobResourcePath => Checked[JobResource])
    (implicit s: Scheduler, iox: IOExecutor) =
    Props { new JobActor(jobConf, executorConf, pathToJobResource) }

  object Response {
    final case class OrderProcessed(orderId: OrderId, outcome: Outcome)
  }

  object Input {
    case object OrderAvailable
    final case class Terminate(signal: Option[ProcessSignal] = None)
    final case class KillProcess(orderId: OrderId, signal: Option[ProcessSignal])
    final case class ProcessOrder(
      order: Order[Order.Processing],
      defaultArguments: Map[String, Expression],
      stdObservers: StdObservers)
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class Step(processOrder: ProcessOrder, jobExecutor: JobExecutor, checkedStart: Checked[Unit])
    final case class Process(processOrder: ProcessOrder, checkedOrderProcess: Checked[OrderProcess])
    final case class TaskFinished(orderId: OrderId, outcome: Outcome)
    case object KillAll extends DeadLetterSuppression
    final case class Timeout(orderId: OrderId) extends DeadLetterSuppression
    final case class KillOrder(orderId: OrderId) extends DeadLetterSuppression
  }

  private final class Entry(val orderProcess: OrderProcess) {
    val timeoutSchedule = SerialCancelable()
    var runningSince: MonixDeadline = null
    var isKilled = false
    var timedOut = false
  }
}
