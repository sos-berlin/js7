package js7.agent.scheduler.order

import akka.actor.ActorRef
import js7.agent.scheduler.order.JobRegister._
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.utils.DuplicateKeyException
import js7.base.utils.ScalaUtils.syntax._
import js7.core.common.ActorRegister
import js7.data.job.JobKey
import js7.data.order.OrderId
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class JobRegister extends ActorRegister[JobKey, JobEntry](_.actor) {

  override protected def noSuchKeyProblem(jobKey: JobKey) = Problem.pure(s"No such Job: $jobKey")

  def onActorTerminated(actor: ActorRef): Unit =
    for (jobEntry <- remove(actorToKey(actor))) {
      logger.trace(s"Removing ${jobEntry.jobKey} after Actor death")
    }

  def insert(key: JobKey, workflowJob: WorkflowJob, actor: ActorRef): Unit =
    this.insert(key -> new JobEntry(key, workflowJob, actor))
}

object JobRegister {
  private val logger = Logger(getClass)

  final class JobEntry private[JobRegister](val jobKey: JobKey, val workflowJob: WorkflowJob, val actor: ActorRef) {
    val queue = new OrderQueue
    var waitingForOrder = false
  }

  final class OrderQueue private[order] {
    private val queue = mutable.ListBuffer.empty[OrderId]
    private val queueSet = mutable.Set.empty[OrderId]
    private val inProcess = mutable.Set.empty[OrderId]

    def dequeue(): Option[OrderId] =
      queue.nonEmpty option {
        val orderId = queue.remove(0)
        queueSet -= orderId
        inProcess += orderId
        orderId
      }

    def +=(orderId: OrderId) = {
      if (inProcess(orderId)) throw new DuplicateKeyException(s"Duplicate $orderId")
      if (queueSet contains orderId) throw new DuplicateKeyException(s"Duplicate $orderId")
      queue += orderId
      queueSet += orderId
    }

    def -=(orderId: OrderId) =
      if (!inProcess.remove(orderId)) {
        val s = queue.size
        queue -= orderId
        if (queue.size == s) {
          logger.warn(s"JobRegister.OrderQueue: unknown $orderId")
        }
        queueSet -= orderId
      }
  }
}
