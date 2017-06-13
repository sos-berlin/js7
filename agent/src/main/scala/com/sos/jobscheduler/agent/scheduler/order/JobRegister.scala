package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.scheduler.order.JobRegister._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.shared.common.ActorRegister
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class JobRegister extends ActorRegister[JobPath, JobEntry](_.actor) {

  def onActorTerminated(actor: ActorRef): Unit = {
    for (jobEntry ← remove(actorToKey(actor))) {
      logger.debug(s"Removing ${jobEntry.jobPath} after Actor death")
    }
  }

  def insert(jobPath: JobPath, actor: ActorRef): Unit = {
    this.insert(jobPath → new JobEntry(jobPath, actor))
  }
}

object JobRegister {
  private val logger = Logger(getClass)

  final class JobEntry private[JobRegister](val jobPath: JobPath, val actor: ActorRef) {
    val queue = new OrderQueue
    var waitingForOrder = false
  }

  final class OrderQueue {
    private var queue: mutable.ListBuffer[OrderId] = mutable.ListBuffer()
    private var inProcess = mutable.Set[OrderId]()

    def dequeue(): Option[OrderId] =
      for (orderId ← queue find { o ⇒ !inProcess(o) }) yield {
        inProcess += orderId
        orderId
      }

    def +=(orderId: OrderId) = queue += orderId

    def -=(orderId: OrderId) = {
      queue -= orderId
      inProcess -= orderId
    }
  }
}
