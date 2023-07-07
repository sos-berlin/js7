package js7.data.item

import js7.data.item.InventoryItemDiff.*

final case class InventoryItemDiff[P <: InventoryItemPath, I <: InventoryItem](
  addedOrChanged: Seq[I] = Nil,
  removed: Seq[P] = Nil)
{
  /** For tests: ordering is irrelevant. */
  override def equals(other: Any) = other match {
    case o: InventoryItemDiff[?, ?] =>
      addedOrChanged.toSet == o.addedOrChanged.toSet &&
        removed.toSet == removed.toSet
    case _ => false
  }

  def isEmpty = addedOrChanged.isEmpty && removed.isEmpty

  /** Returns a subset of a certain `InventoryItemPath` and `InventoryItem`. */
  def select[P1 <: P, I1 <: I](implicit I1: InventoryItem.Companion[I1]): InventoryItemDiff[P1, I1] =
    InventoryItemDiff(
      addedOrChanged   collect { case o if o.companion eq I1 => o.asInstanceOf[I1] },
      removed collect { case o if o.companion eq I1.Path => o.asInstanceOf[P1] })

  def withVersionId(versionId: VersionId): InventoryItemDiff[P, I] =
    copy(
      addedOrChanged = addedOrChanged.map(itemWithVersionId(_, versionId)))

  def containsVersionedItem: Boolean =
    addedOrChanged.exists(_.isInstanceOf[VersionedItem]) ||
      removed.exists(_.isInstanceOf[VersionedItemPath])
}
object InventoryItemDiff
{
  def diff(as: Iterable[InventoryItem], bs: Iterable[InventoryItem], ignoreVersion: Boolean)
  : InventoryItemDiff_ =
    fromItemChanges(diffItems(as, bs, ignoreVersion = ignoreVersion))

  def fromItemChanges(events: Seq[ItemChange]) =
    InventoryItemDiff(
      events collect { case o: ItemChange.AddedOrChanged => o.item },
      events collect { case o: ItemChange.Removed => o.path })

  def diffItems(
    changed: Iterable[InventoryItem],
    base: Iterable[InventoryItem],
    ignoreVersion: Boolean = false)
  : Seq[ItemChange] = {
    val pathToMaybeItem = base.view
      .map(o => o.path -> o)
      .toMap[InventoryItemPath, InventoryItem]
      .lift
    val addedOrChanged = changed.view.filter(isAddedOrChanged(_, pathToMaybeItem, ignoreVersion))
    val changedItemPaths = changed.view.map(_.path).toSet
    val removedItems = base.view.map(_.path).filterNot(changedItemPaths)
    removedItems.map(ItemChange.Removed(_))
      .concat(addedOrChanged.map(ItemChange.AddedOrChanged(_)))
      .toVector
      .sortBy(_.path)
  }

  private def isAddedOrChanged(
    item: InventoryItem,
    previousPathToItem: InventoryItemPath => Option[InventoryItem],
    ignoreVersion: Boolean)
  : Boolean =
    (item, previousPathToItem(item.path)) match {
      case (_, None) =>
        true
      case (item: SimpleItem, Some(previous: SimpleItem)) if ignoreVersion =>
        item.withRevision(previous.itemRevision) != previous
      case (item: VersionedItem, Some(previous: VersionedItem)) if ignoreVersion =>
        item.withVersion(previous.key.versionId) != previous
      case (item, Some(previous)) =>
        item != previous
    }

  private def itemWithVersionId[I <: InventoryItem](item: I, v: VersionId): I =
    item match {
      case item: VersionedItem => item.withVersion(v).asInstanceOf[I]
      case o => o
    }
}
