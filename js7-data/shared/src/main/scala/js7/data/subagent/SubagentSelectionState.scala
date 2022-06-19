package js7.data.subagent

import js7.data.item.{TrivialItemState, UnsignedSimpleItemState}

/** Just for orthogonality. SubagentSelection has no State. */
final case class SubagentSelectionState(item: SubagentSelection)
extends UnsignedSimpleItemState with TrivialItemState {
  protected type Self = SubagentSelectionState
  val companion = SubagentSelectionState
}

object SubagentSelectionState extends UnsignedSimpleItemState.Companion[SubagentSelectionState]
{
  type Path = SubagentSelectionId
  type ItemState = SubagentSelectionState
  type Item = SubagentSelection
}
