package js7.data.orderwatch

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.item.UnsignedSimpleItem
import js7.data.orderwatch.OrderWatch._
import js7.data.workflow.WorkflowPath

trait OrderWatch extends UnsignedSimpleItem
{
  type Self <: OrderWatch

  val companion: Companion[Self] { type Key <: OrderWatchPath }

  val agentPath: AgentPath
  val workflowPath: WorkflowPath
}

object OrderWatch
{
  trait Companion[A <: OrderWatch] extends UnsignedSimpleItem.Companion[A]
  {
    type Key <: OrderWatchPath
  }

  implicit val jsonCodec = TypedJsonCodec[OrderWatch](
    Subtype[FileWatch])
}
