package js7.data.orderwatch

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentId
import js7.data.item.SimpleItem
import js7.data.orderwatch.OrderWatch._
import js7.data.workflow.WorkflowPath

trait OrderWatch extends SimpleItem
{
  val companion: Companion { type Id <: OrderWatchId }

  val agentId: AgentId
  val workflowPath: WorkflowPath
}

object OrderWatch
{
  trait Companion extends SimpleItem.Companion
  {
    type Item <: OrderWatch
    type Id <: OrderWatchId
  }

  implicit val jsonCodec = TypedJsonCodec[OrderWatch](
    Subtype[FileWatch])
}
