package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.jobnet.{Jobnet, JobnetEvent, JobnetPath, NodeKey}
import com.sos.jobscheduler.data.order.Order
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class JobnetRegister {

  private val pathToJobnet = mutable.Map[JobnetPath, Jobnet]()
    .withDefault { jobnetPath ⇒ throw new NoSuchElementException(s"Unknown $jobnetPath") }

  def recover(jobnet: Jobnet): Unit = {
    pathToJobnet.insert(jobnet.path → jobnet)
  }

  def handleEvent(keyedEvent: KeyedEvent[JobnetEvent]): Unit = {
    val path = keyedEvent.key
    keyedEvent.event match {
      case event: JobnetEvent.JobnetAttached ⇒
        pathToJobnet += path → Jobnet.fromJobnetAttached(path, event)   // Multiple orders with same Jobnet may occur. TODO Every Order becomes its own copy of its Jobnet? Jobnet will never be removed.
    }
  }

  /** Reuses string from jobnet to avoid duplicate strings */
  def reuseMemory[S <: Order.State](order: Order[S]): Order[S] = {
    // A more general place for object identification may be the JSON deserializer: it needs access to an reusable object pool
    val jobnet = pathToJobnet(order.jobnetPath)
    val nk = jobnet.idToNode.get(order.nodeId) match {  // Agent gets a Jobnet fragment without node IDs for other agents
      case Some(node) ⇒ NodeKey(jobnet.path, node.id)
      case None ⇒ NodeKey(jobnet.path, order.nodeId)
    }
    if (order.nodeKey eq nk)
      order
    else {
      assert(order.nodeKey == nk)
      order.copy(nodeKey = nk)
    }
  }

  def nodeKeyToJobNodeOption(nodeKey: NodeKey): Option[Jobnet.JobNode] =
    nodeKeyToNodeOption(nodeKey) collect { case o: Jobnet.JobNode ⇒ o }

  def nodeKeyToNodeOption(nodeKey: NodeKey): Option[Jobnet.Node] =
    pathToJobnet(nodeKey.jobnetPath).idToNode.get(nodeKey.nodeId)

  def get(path: JobnetPath): Option[Jobnet] =
    pathToJobnet.get(path)

  def apply(path: JobnetPath): Jobnet =
    pathToJobnet(path)

  def jobnets: Vector[Jobnet] =
    pathToJobnet.values.toVector

  def size: Int =
    pathToJobnet.size
}
