package com.sos.scheduler.engine.data.compounds

import com.sos.scheduler.engine.data.job.{JobOverview, ProcessClassOverview, TaskOverview}
import com.sos.scheduler.engine.data.jobchain.{JobChainOverview, JobNodeOverview}
import com.sos.scheduler.engine.data.order.OrderView
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

/**
  * @author Joacim Zschimmer
  */
final case class OrdersComplemented[V <: OrderView](
  orders: immutable.Seq[V],
  usedJobChains: immutable.Seq[JobChainOverview],
  usedNodes: immutable.Seq[JobNodeOverview],
  usedJobs: immutable.Seq[JobOverview],
  usedTasks: immutable.Seq[TaskOverview],
  usedProcessClasses: immutable.Seq[ProcessClassOverview])

object OrdersComplemented {
  implicit def jsonFormat[V <: OrderView: OrderView.Companion: RootJsonFormat] = jsonFormat6(OrdersComplemented[V])
}
