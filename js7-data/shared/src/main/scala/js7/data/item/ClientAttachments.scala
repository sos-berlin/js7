package js7.data.item

import cats.effect.IO
import fs2.Stream
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.cast
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.delegate.DelegateId
import js7.data.event.EventDriven
import js7.data.item.BasicItemEvent.{ItemAttachedStateEvent, ItemDeleted}
import js7.data.item.ClientAttachments.*
import js7.data.item.ItemAttachedState.{Detached, NotDetached}
import scala.collection.View
import scala.reflect.ClassTag

/** Client side bookkeeping of attachments. */
final case class ClientAttachments[D <: DelegateId: ClassTag: Tag](
  itemToDelegateToAttachedState: Map[InventoryItemKey, Map[D, ItemAttachedState.NotDetached]])
extends
  EventDriven[ClientAttachments[D], ItemAttachedStateEvent]:

  def companion = ClientAttachments
    .asInstanceOf[EventDriven.Companion[ClientAttachments[D], ItemAttachedStateEvent]] // ???

  def estimatedSnapshotSize: Int =
    itemToDelegateToAttachedState.values.view.map(_.size).sum

  def toSnapshotStream: Stream[fs2.Pure, ItemAttachedStateEvent] =
    Stream.iterable(itemToDelegateToAttachedState
      .to(View)
      .flatMap:(key, agentToAttached) =>
        agentToAttached.map: (agentPath, attachedState) =>
          ItemAttachedStateEvent(key, agentPath, attachedState))

  def applyEvent(event: ItemAttachedStateEvent): Checked[ClientAttachments[D]] =
    val delegateId = cast[D](event.delegateId)
    import event.{attachedState, key as itemKey}
    attachedState match
      case attachedState: NotDetached =>
        Right(copy(
          itemToDelegateToAttachedState = itemToDelegateToAttachedState +
            (itemKey ->
              (itemToDelegateToAttachedState.getOrElse(itemKey, Map.empty) +
                (delegateId -> attachedState)))))

      case Detached =>
        for
          agentToAttachedState <- itemToDelegateToAttachedState.checked(itemKey)
          _ <- agentToAttachedState.checked(delegateId)
        yield
          copy(itemToDelegateToAttachedState = {
            val updated = agentToAttachedState - delegateId
            if updated.isEmpty then
              itemToDelegateToAttachedState - itemKey
            else
              itemToDelegateToAttachedState + (itemKey -> updated)
          })

  def applyItemDeleted(event: ItemDeleted): ClientAttachments[D] =
    event.key match
      case delegateId: DelegateId =>
        copy(
          itemToDelegateToAttachedState = itemToDelegateToAttachedState
            .view
            .flatMap { case (itemKey, agentToAttached) =>
              agentToAttached
                .view
                .filterNot { case (a, notDetached) =>
                  (a == delegateId) && {
                    logger.debug(
                      s"$event: silently detach ${notDetached.toShortString} $itemKey")
                    true
                  }
                }
                .toMap
                .ifNonEmpty.map(itemKey -> _)
            }
            .toMap)

      case itemKey =>
        copy(
          itemToDelegateToAttachedState = itemToDelegateToAttachedState - itemKey)


object ClientAttachments
extends EventDriven.Companion[ClientAttachments[?], ItemAttachedStateEvent]:

  private val Empty = ClientAttachments[DelegateId](Map.empty)

  def empty[D <: DelegateId]: ClientAttachments[D] =
    Empty.asInstanceOf[ClientAttachments[D]]

  private val logger = Logger[this.type]
