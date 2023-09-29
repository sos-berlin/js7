package js7.data.orderwatch

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentPath
import js7.data.item.UnsignedSimpleItem
import js7.data.orderwatch.OrderWatch.*
import js7.data.workflow.WorkflowPath

trait OrderWatch extends UnsignedSimpleItem:
  protected type Self <: OrderWatch

  val companion: Companion[Self] { type Key <: OrderWatchPath }

  def toInitialItemState: OrderWatchState =
    OrderWatchState(this)

  val agentPath: AgentPath

  val workflowPath: WorkflowPath

  override def dedicatedAgentPath =
    Some(agentPath)

object OrderWatch:
  trait Companion[A <: OrderWatch] extends UnsignedSimpleItem.Companion[A]:
    type Key = OrderWatchPath
    def Key = OrderWatchPath

    type ItemState = OrderWatchState

  implicit val jsonCodec: TypedJsonCodec[OrderWatch] = TypedJsonCodec(
    Subtype[FileWatch])
