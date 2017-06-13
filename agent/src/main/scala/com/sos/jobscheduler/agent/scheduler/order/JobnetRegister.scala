package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.common.scalautil.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.jobnet.{Jobnet, JobnetEvent, JobnetPath, NodeKey}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
private[order] final class JobnetRegister {

  private val pathToJobnet = mutable.Map[JobnetPath, Jobnet]()
    .withDefault { jobnetPath ⇒ throw new NoSuchElementException(s"Unknown $jobnetPath")}

  def recover(jobnet: Jobnet): Unit = {
    pathToJobnet.insert(jobnet.path → jobnet)
  }

  def handleEvent(keyedEvent: KeyedEvent[JobnetEvent]): Unit = {
    val path = keyedEvent.key
    keyedEvent.event match {
      case event: JobnetEvent.JobnetAttached ⇒
        pathToJobnet += path → Jobnet.fromJobnetAttached(path, event)   // Multiple orders with same Jobnet may occur. TODO Every Order becomes its own copy of its Jobnet ?
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
