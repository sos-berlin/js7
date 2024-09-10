package js7.data.subagent

import js7.data.item.{SeparateTrivialItemState, UnsignedSimpleItemState}

/** Just for orthogonality. SubagentBundle has no State. */
final case class SubagentBundleState(item: SubagentBundle)
extends UnsignedSimpleItemState, SeparateTrivialItemState[SubagentBundleState]:

  protected type Self = SubagentBundleState
  val companion: SubagentBundleState.type = SubagentBundleState


object SubagentBundleState extends UnsignedSimpleItemState.Companion[SubagentBundleState]:
  type Key = SubagentBundleId
  type Item = SubagentBundle
  override type ItemState = SubagentBundleState
