package js7.data.orderwatch

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.agent.AgentPath
import js7.data.item.{UnsignedSimpleItem, UnsignedSimpleItemPath}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatch.*
import js7.data.plan.PlanId
import js7.data.workflow.WorkflowPath

trait OrderWatch extends UnsignedSimpleItem:
  protected type Self <: OrderWatch

  val companion: Companion[Self] { type Key <: OrderWatchPath }

  def toInitialItemState: OrderWatchState =
    OrderWatchState(this)

  val agentPath: AgentPath

  val workflowPath: WorkflowPath

  def externalToOrderAndPlanId(externalOrderName: ExternalOrderName, legacyOrderId: Option[OrderId], now: Timestamp)
  : Checked[(OrderId, PlanId)]

  override def dedicatedAgentPath: Option[AgentPath] =
    Some(agentPath)


object OrderWatch:
  trait Companion[A <: OrderWatch] extends UnsignedSimpleItem.Companion[A]:
    type Key = OrderWatchPath

    def Key: UnsignedSimpleItemPath.Companion[OrderWatchPath] =
      OrderWatchPath

    type ItemState = OrderWatchState

  implicit val jsonCodec: TypedJsonCodec[OrderWatch] = TypedJsonCodec(
    Subtype[FileWatch])
