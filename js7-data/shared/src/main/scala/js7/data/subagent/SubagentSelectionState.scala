package js7.data.subagent

import js7.data.item.{SeparateTrivialItemState, UnsignedSimpleItemState}

/** Just for orthogonality. SubagentSelection has no State. */
final case class SubagentSelectionState(item: SubagentSelection)
extends UnsignedSimpleItemState with SeparateTrivialItemState[SubagentSelectionState]:
  protected type Self = SubagentSelectionState
  val companion: SubagentSelectionState.type = SubagentSelectionState


object SubagentSelectionState extends UnsignedSimpleItemState.Companion[SubagentSelectionState]:
  type Key = SubagentSelectionId
  type Item = SubagentSelection
  override type ItemState = SubagentSelectionState
