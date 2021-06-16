package js7.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import cats.syntax.traverse._
import js7.agent.scheduler.job.JobActor._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
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
import scala.collection.mutable
import scala.util.{Failure, Success}

private[agent] final class JobActor private(
  jobConf: JobConf,
  jobExecutorConf: JobExecutorConf,
  pathToJobResource: JobResourcePath => Checked[JobResource])
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends Actor with Stash
{
  import jobConf.{jobKey, sigKillDelay, workflow, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.name)
  private val orderToProcess = mutable.Map.empty[OrderId, OrderProcess]
  private val isOrderKilled = mutable.Set.empty[OrderId]
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
          replyWithOrderProcessed(
            Response.OrderProcessed(order.id, Outcome.Disrupted(problem), isOrderKilled(order.id)))

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
          replyWithOrderProcessed(
            Response.OrderProcessed(order.id, Outcome.Disrupted(problem), isOrderKilled(order.id)))

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
      orderToProcess.insert(order.id -> orderProcess)
      orderProcess
        .runToFuture(processOrder.stdObservers)  // runToFuture completes the out/err observers
        .onComplete { tried =>
          val outcome = tried match {
            case Success(outcome: Succeeded) =>
              processOrder.stdObservers.errorLine match {
                case None => outcome
                case Some(errorLine) =>
                  assert(workflowJob.failOnErrWritten)  // see OrderActor
                  Outcome.Failed(Some(s"The job's error channel: $errorLine"))
              }
            case Success(o) => o
            case Failure(t) =>
              logger.error(s"${order.id}: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
              Outcome.Failed.fromThrowable(t)
          }
          self.!(Internal.TaskFinished(order.id, outcome))(sender)
        }


    case Internal.TaskFinished(orderId, outcome) =>
      orderToProcess -= orderId
      val killed = isOrderKilled.remove(orderId)
      replyWithOrderProcessed(Response.OrderProcessed(orderId, outcome, isKilled = killed))

    case Input.Terminate(maybeSignal) =>
      logger.debug("Terminate")
      terminating = true
      for (signal <- maybeSignal) {
        killAll(signal)
      }
      if (!maybeSignal.contains(SIGKILL) && orderProcessCount > 0) {
        scheduler.scheduleOnce(sigKillDelay) {
          self ! Internal.KillAll
        }
      }
      continueTermination()

    case Input.KillProcess(orderId, maybeSignal) =>
      for (signal <- maybeSignal) {
        killOrder(orderId, signal)
      }
      if (!maybeSignal.contains(SIGKILL) && orderProcessCount > 0) {
        scheduler.scheduleOnce(sigKillDelay) {
          self ! Internal.KillOrder(orderId)
        }
      }

    case Internal.KillAll =>
      killAll(SIGKILL)

    case Internal.KillOrder(orderId) =>
      killOrder(orderId, SIGKILL)
  }

  private def replyWithOrderProcessed(msg: Response.OrderProcessed): Unit = {
    sender() ! msg
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
        killOrder(orderId, signal)
      }
    }

  private def killOrder(orderId: OrderId, signal: ProcessSignal): Unit =
    for (orderProcess <- orderToProcess.get(orderId)) {
      logger.info(s"Kill $signal $orderId")
      isOrderKilled += orderId

      Task
        .defer/*catches exception*/ {
          orderProcess.cancel(immediately = signal == SIGKILL)
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
    final case class OrderProcessed(orderId: OrderId, outcome: Outcome, isKilled: Boolean)
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
    final case object KillAll extends DeadLetterSuppression
    final case class KillOrder(orderId: OrderId) extends DeadLetterSuppression
  }
}
