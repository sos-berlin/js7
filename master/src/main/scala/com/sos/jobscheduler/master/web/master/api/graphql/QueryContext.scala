package com.sos.jobscheduler.master.web.master.api.graphql

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.filebased.FileBased
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import java.util.regex.Pattern
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait QueryContext {
  implicit def executionContext: ExecutionContext
  def order(orderId: OrderId): Future[Option[Order[Order.State]]]
  def orders(filter: QueryContext.OrderFilter = QueryContext.OrderFilter.Default): Future[Seq[Order[Order.State]]]
  def idTo[A <: FileBased: FileBased.Companion](id: A#Id): Future[Checked[A]]
}
object QueryContext {
  final case class OrderFilter(
    limit: Int = Int.MaxValue,
    workflowPath: Option[WorkflowPath] = None,
    idPattern: Option[Pattern] = None)
  object OrderFilter {
    val Default = OrderFilter()
  }
}
