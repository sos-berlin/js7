package js7.data.subagent

import js7.data.item.UnsignedSimpleItemState
import monix.reactive.Observable

/** Just for orthogonality. SubagentSelection has no State. */
final case class SubagentSelectionState(item: SubagentSelection) extends UnsignedSimpleItemState {
  protected type Item = SubagentSelection
  protected type Self = SubagentSelectionState
  val companion = SubagentSelectionState

  override def toSnapshotObservable = Observable.pure(item)
}

object SubagentSelectionState extends UnsignedSimpleItemState.Companion[SubagentSelectionState]
{
  type Path = SubagentSelectionId
}
