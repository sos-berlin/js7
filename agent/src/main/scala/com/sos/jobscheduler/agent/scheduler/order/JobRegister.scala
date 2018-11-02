package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.JobRegister._
import com.sos.jobscheduler.base.utils.DuplicateKeyException
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.order.OrderId
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class JobRegister extends ActorRegister[JobKey, JobEntry](_.actor) {

  override protected def noSuchKeyMessage(jobKey: JobKey) = s"No such job '$jobKey'"

  def onActorTerminated(actor: ActorRef): Unit =
    for (jobEntry ← remove(actorToKey(actor))) {
      logger.debug(s"Removing ${jobEntry.jobKey} after Actor death")
    }

  def insert(key: JobKey, actor: ActorRef): Unit =
    this.insert(key → new JobEntry(key, actor))
}

object JobRegister {
  private val logger = Logger(getClass)

  final class JobEntry private[JobRegister](val jobKey: JobKey, val actor: ActorRef) {
    val queue = new OrderQueue
    var waitingForOrder = false
  }

  final class OrderQueue private[order] {
    private var queue = mutable.ListBuffer[OrderId]()
    private var inProcess = mutable.Set[OrderId]()

    def dequeue(): Option[OrderId] =
      queue.nonEmpty option {
        val orderId = queue.remove(0)
        inProcess += orderId
        orderId
      }

    def +=(orderId: OrderId) = {
      if (inProcess(orderId)) throw new DuplicateKeyException(s"Duplicate $orderId")
      if (queue contains orderId) throw new DuplicateKeyException(s"Duplicate $orderId")
      queue += orderId
    }

    def -=(orderId: OrderId) =
      if (!inProcess.remove(orderId)) {
        val s = queue.size
        queue -= orderId
        if (queue.size == s) {
          logger.warn(s"JobRegister.OrderQueue: unknown $orderId")
        }
      }
  }
}
