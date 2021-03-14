package js7.data.ordersource

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.data.agent.AgentId
import js7.data.item.SimpleItem
import js7.data.order.OrderId
import js7.data.ordersource.OrderSource._
import js7.data.workflow.WorkflowPath

trait OrderSource extends SimpleItem
{
  val companion: Companion { type Id <: OrderSourceId }

  val agentId: AgentId
  val workflowPath: WorkflowPath

  def generateOrderId(sourceOrderName: SourceOrderName): Checked[OrderId]
}

object OrderSource
{
  trait Companion extends SimpleItem.Companion
  {
    type Item <: OrderSource
    type Id <: OrderSourceId
  }

  implicit val jsonCodec = TypedJsonCodec[OrderSource](
    Subtype[FileOrderSource])
}
