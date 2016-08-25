package com.sos.scheduler.engine.data.compounds

import com.sos.scheduler.engine.data.folder.FolderTree
import com.sos.scheduler.engine.data.job.{JobOverview, ProcessClassOverview, TaskOverview}
import com.sos.scheduler.engine.data.jobchain.JobNodeOverview
import com.sos.scheduler.engine.data.order.OrderView
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonWriter

/**
  * @author Joacim Zschimmer
  */
final case class OrderTreeComplemented[V <: OrderView](
  orderTree: FolderTree[V],
  usedNodes: immutable.Seq[JobNodeOverview],
  usedJobs: immutable.Seq[JobOverview],
  usedTasks: immutable.Seq[TaskOverview],
  usedProcessClasses: immutable.Seq[ProcessClassOverview])

object OrderTreeComplemented {
  implicit def jsonFormat[V <: OrderView: OrderView.Companion: RootJsonWriter] = jsonFormat5(OrderTreeComplemented[V])
}
