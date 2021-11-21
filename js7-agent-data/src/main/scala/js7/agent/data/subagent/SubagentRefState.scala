package js7.agent.data.subagent

import js7.base.problem.Checked
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemAttachedStateEvent, ItemDetachable, ItemDetached}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable, NotDetached}
import js7.data.item.{InventoryItemKey, InventoryItemState}
import js7.data.subagent.SubagentRef
import monix.reactive.Observable

final case class SubagentRefState(
  subagentRef: SubagentRef,
  itemToAttachedState: Map[InventoryItemKey, NotDetached])
extends InventoryItemState
//extends EventDrivenState[SubagentRefState, SubagentRefEvent]
{
  protected type Item = SubagentRef

  def item = subagentRef

  def subagentId =
    subagentRef.id

  def estimatedSnapshotSize =
    1 + itemToAttachedState.size

  def toSnapshotObservable =
    Observable.pure(subagentRef) ++
      Observable
        .fromIterable(itemToAttachedState)
        .map { case (itemKey, attachedState) =>
          ItemAttachedStateEvent(itemKey, subagentId, attachedState)
        }

  def applyEvent(event: ItemAttachedStateEvent): Checked[SubagentRefState] =
    event match {
      case ItemAttachable(itemKey, subagentRef.id) =>
        updateItemAttachedState(itemKey, Attachable)

      case ItemAttached(itemKey, itemRevision, subagentRef.id) =>
        updateItemAttachedState(itemKey, Attached(itemRevision))

      case ItemDetachable(itemKey, subagentRef.id) =>
        updateItemAttachedState(itemKey, Detachable)

      case ItemDetached(itemKey, subagentRef.id) =>
        Right(copy(
          itemToAttachedState = itemToAttachedState - itemKey))
    }

  private def updateItemAttachedState(itemKey: InventoryItemKey, attachedState: NotDetached) =
    Right(copy(
      itemToAttachedState = itemToAttachedState + (itemKey -> attachedState)
    ))
}

object SubagentRefState
{
  def initial(subagentRef: SubagentRef) =
    SubagentRefState(subagentRef, Map.empty)
}
