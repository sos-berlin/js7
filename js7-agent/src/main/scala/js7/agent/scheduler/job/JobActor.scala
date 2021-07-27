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
import scala.concurrent.Future
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
  private var waitingForNextOrders = 0
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

    case Input.ProcessOrder(order, defaultArguments, stdObservers) if waitingForNextOrders > 0 =>
      val sender = this.sender()
      waitingForNextOrders -= 1

      // Read JobResources each time because they may change at any time
      (for {
        jobExecutor <- checkedJobExecutor
        resourcesPaths <- jobConf.jobResourcePaths.traverse(pathToJobResource)
      } yield jobExecutor -> resourcesPaths)
      match {
        case Left(problem) =>
          replyWithOrderProcessed(order.id, Outcome.Disrupted(problem))
          handleIfReadyForOrder()

        case Right((jobExecutor, jobResources)) =>
          val processOrder = ProcessOrder(
            order, workflow, jobKey, jobResources,
            defaultArgumentExpressions = workflowJob.defaultArguments ++ defaultArguments,
            jobConf.controllerId, stdObservers)
          val entry = new Entry
          orderToProcess.insert(order.id -> entry)
          jobExecutor.start
            .materializeIntoChecked
            .map { checked =>
              self.!(Internal.Step(processOrder, jobExecutor, checked, entry))(sender)
            }
            .runAsyncAndForget
      }

    case Internal.Step(processOrder, jobExecutor, checkedJobContextStart, entry) =>
      import processOrder.order
      checkedJobContextStart match {
        case Left(problem) =>
          replyWithOrderProcessed(order.id, Outcome.Disrupted(problem))
          orderToProcess -= order.id
          handleIfReadyForOrder()

        case Right(()) =>
          val sender = this.sender()
          jobExecutor
            .prepareOrderProcess(processOrder)
            .materializeIntoChecked
            .foreach(o => self.!(Internal.Process(processOrder, entry, o))(sender))
      }
      handleIfReadyForOrder()

    case Internal.Process(processOrder: ProcessOrder, _, Left(problem)) =>
      import processOrder.order
      val sender = this.sender()
      self.!(Internal.TaskFinished(order.id, Outcome.Disrupted(problem)))(sender)

    case Internal.Process(processOrder, entry, Right(orderProcess)) =>
      import processOrder.order
      val sender = this.sender()

      entry.orderProcess = Some(orderProcess)
      // Start the orderProcess.
      // The future completes the out/err observers
      orderProcess.start(processOrder.stdObservers)
        .runToFuture
        .onComplete {
          case Failure(t) =>
            val outcome = Outcome.Failed.fromThrowable(t)
            self.!(Internal.TaskFinished(order.id, outcome))(sender)

          case Success(runningProcessFuture) =>
            self.!(Internal.ProcessStarted(processOrder, runningProcessFuture, entry))(sender)
        }

    case Internal.ProcessStarted(processOrder, runningProcessFuture, entry) =>
      import processOrder.order
      val sender = this.sender()
      entry.runningSince = scheduler.now
      for (t <- timeout) {
        entry.timeoutSchedule := scheduler.scheduleOnce(t) {
          self ! Internal.Timeout(order.id)
        }
      }

      runningProcessFuture.onComplete { tried =>
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

      for (signal <- entry.killSignal) {
        // Input.KillProcess may have arrived after Internal.Process
        // but before Internal.ProcessStarted, so we kill now.
        kill(order.id, signal)
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
      handleIfReadyForOrder()

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
  }

  private def handleIfReadyForOrder(): Unit = {
    val available = workflowJob.parallelism - orderProcessCount
    if (waitingForNextOrders == 0 && !terminating && available > 0) {
      context.parent ! Output.ReadyForOrder(available)
      waitingForNextOrders = available
    }
  }

  private def killAll(signal: ProcessSignal): Unit =
    if (orderToProcess.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $orderProcessCount tasks")
      for (orderId <- orderToProcess.keys.toVector/*copy*/) {
        kill(orderId, signal)
      }
    }

  private def killOrder(orderId: OrderId, maybeSignal: Option[ProcessSignal]): Unit = {
    val signal = if (sigkillDelay.isZeroOrBelow) Some(SIGKILL) else maybeSignal
    for (signal <- signal) {
      for (entry <- orderToProcess.get(orderId)) {
        entry.killSignal = Some(signal)
      }
      kill(orderId, signal)
    }
    if (!signal.contains(SIGKILL) && orderToProcess.contains(orderId)) {
      scheduler.scheduleOnce(sigkillDelay) {
        self ! Internal.KillOrder(orderId)
      }
    }
  }

  private def kill(orderId: OrderId, signal: ProcessSignal): Unit =
    for (entry <- orderToProcess.get(orderId)) {
      for (orderProcess <- entry.orderProcess) {
        logger.info(s"Kill $signal $orderId")
        entry.isKilled = true

        Task
          .defer/*catch exception*/ {
            orderProcess.cancel(immediately = signal == SIGKILL)
          }
          .onErrorHandle(t => logger.error(
            s"Kill $orderId: ${t.toStringWithCauses}", t.nullIfNoStackTrace))
          .runAsyncAndForget
      }
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
    final case class ReadyForOrder(n: Int)
  }

  private object Internal {
    final case class Step(processOrder: ProcessOrder, jobExecutor: JobExecutor, checkedStart: Checked[Unit], entry: Entry)
    final case class Process(processOrder: ProcessOrder, entry: Entry, checkedOrderProcess: Checked[OrderProcess])

    final case class ProcessStarted(
      processOrder: ProcessOrder,
      runningProcessFuture: Future[Outcome.Completed],
      entry: Entry)

    final case class TaskFinished(orderId: OrderId, outcome: Outcome)
    case object KillAll extends DeadLetterSuppression
    final case class Timeout(orderId: OrderId) extends DeadLetterSuppression
    final case class KillOrder(orderId: OrderId) extends DeadLetterSuppression
  }

  private final class Entry {
    var orderProcess: Option[OrderProcess] = None
    var killSignal: Option[ProcessSignal] = None
    val timeoutSchedule = SerialCancelable()
    var runningSince: MonixDeadline = null
    var isKilled = false
    var timedOut = false
  }
}
