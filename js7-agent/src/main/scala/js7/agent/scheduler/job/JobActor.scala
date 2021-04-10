package js7.agent.scheduler.job

import akka.actor.{Actor, DeadLetterSuppression, Props, Stash}
import js7.agent.scheduler.job.JobActor._
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.JobConf
import js7.data.order.{OrderId, Outcome}
import js7.executor.configuration.JobExecutorConf
import js7.executor.internal.JobExecutor
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

final class JobActor private(
  jobConf: JobConf,
  executorConf: JobExecutorConf)
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends Actor with Stash
{
  import jobConf.{jobKey, sigKillDelay, workflowJob}

  private val logger = Logger.withPrefix[this.type](jobKey.keyName)
  private val orderToProcess = mutable.Map.empty[OrderId, OrderProcess]
  private val isOrderKilled = mutable.Set.empty[OrderId]
  private var waitingForNextOrder = false
  private var terminating = false

  private val checkedJobExecutor: Checked[JobExecutor] =
    JobExecutor.checked(jobConf, executorConf)

  for (problem <- checkedJobExecutor.left) logger.error(problem.toString)

  override def postStop() = {
    killAll(SIGKILL)
    super.postStop()
  }

  def receive = {
    case Input.OrderAvailable =>
      handleIfReadyForOrder()

    case processOrder: ProcessOrder if waitingForNextOrder =>
      import processOrder.order
      val sender = this.sender()
      waitingForNextOrder = false
      checkedJobExecutor match {
        case Left(problem) =>
          replyWithOrderProcessed(
            Response.OrderProcessed(order.id, Outcome.Disrupted(problem), isOrderKilled(order.id)))

        case Right(jobExecutor: JobExecutor) =>
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

          jobExecutor.processOrder(processOrder) match {
            case Left(problem) =>
              logger.error(s"Order '${order.id.string}' step could not be started: $problem")
              self.!(Internal.TaskFinished(order.id, Outcome.Disrupted(problem)))(sender)

            case Right(orderProcess) =>
              orderToProcess.insert(order.id -> orderProcess)
              orderProcess
                .runToFuture
                .onComplete { tried =>
                  val outcome = tried match {
                    case Success(o) => o
                    case Failure(t) =>
                      logger.error(s"${ order }: ${ t.toStringWithCauses }", t.nullIfNoStackTrace)
                      Outcome.Failed.fromThrowable(t)
                  }
                  self.!(Internal.TaskFinished(order.id, outcome))(sender)
                }
          }
      }
      handleIfReadyForOrder()

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
      if (!maybeSignal.contains(SIGKILL) && taskCount > 0) {
        scheduler.scheduleOnce(sigKillDelay) {
          self ! Internal.KillAll
        }
      }
      continueTermination()

    case Input.KillProcess(orderId, maybeSignal) =>
      logger.debug(s"KillProcess $orderId")
      for (signal <- maybeSignal) {
        killOrder(orderId, signal)
      }
      if (!maybeSignal.contains(SIGKILL) && taskCount > 0) {
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
    if (!waitingForNextOrder && !terminating && taskCount < workflowJob.taskLimit) {
      context.parent ! Output.ReadyForOrder
      waitingForNextOrder = true
    }

  private def killAll(signal: ProcessSignal): Unit =
    if (orderToProcess.nonEmpty) {
      logger.warn(s"Terminating, sending $signal to all $taskCount tasks")
      for (orderId <- orderToProcess.keys.toVector/*copy*/) {
        killOrder(orderId, signal)
      }
    }

  private def killOrder(orderId: OrderId, signal: ProcessSignal): Unit =
    for (orderProcess <- orderToProcess.get(orderId)) {
      logger.info(s"Kill $signal $orderId")
      isOrderKilled += orderId

      try orderProcess.kill(immediately = signal == SIGKILL)
      catch { case NonFatal(t) =>
        // InternalJob implemenation may throw
        logger.error(s"Kill $orderId: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
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

  private def taskCount = orderToProcess.size
}

object JobActor
{
  def props(jobConf: JobConf, executorConf: JobExecutorConf)
    (implicit s: Scheduler, iox: IOExecutor) =
    Props { new JobActor(jobConf, executorConf) }

  object Response {
    final case class OrderProcessed(orderId: OrderId, outcome: Outcome, isKilled: Boolean)
  }

  object Input {
    case object OrderAvailable
    final case class Terminate(signal: Option[ProcessSignal] = None)
    final case class KillProcess(orderId: OrderId, signal: Option[ProcessSignal])
  }

  object Output {
    case object ReadyForOrder
  }

  private object Internal {
    final case class Step(processOrder: ProcessOrder, jobExecutor: JobExecutor, checkedStart: Checked[Unit])
    final case class TaskFinished(orderId: OrderId, outcome: Outcome)
    final case object KillAll extends DeadLetterSuppression
    final case class KillOrder(orderId: OrderId) extends DeadLetterSuppression
  }
}
