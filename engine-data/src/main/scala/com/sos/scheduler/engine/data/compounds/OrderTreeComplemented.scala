package com.sos.scheduler.engine.data.compounds

import com.sos.scheduler.engine.data.folder.FolderTree
import com.sos.scheduler.engine.data.job.{JobOverview, ProcessClassOverview, TaskOverview}
import com.sos.scheduler.engine.data.order.OrderOverview
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class OrderTreeComplemented(
  orderTree: FolderTree[OrderOverview],
  usedTasks: immutable.Seq[TaskOverview],
  usedJobs: immutable.Seq[JobOverview],
  usedProcessClasses: immutable.Seq[ProcessClassOverview])

object OrderTreeComplemented {
  implicit val MyJsonFormat = jsonFormat4(apply)
}
