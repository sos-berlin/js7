package js7.data.item

/**
  * @author Joacim Zschimmer
  */
object IntentoryItems
{
  def diffInventoryItems(changed: Iterable[InventoryItem], base: Iterable[InventoryItem], ignoreVersion: Boolean = false): Seq[RepoChange] = {
    val pathToMaybeItem = base.view.map(o => o.path -> o).toMap[ItemPath, InventoryItem].lift
    val addedOrChanged = changed.view flatMap toAddedOrChanged(pathToMaybeItem, ignoreVersion)
    val changedPaths = changed.view.map(_.path).toSet
    val deletedEvents = base.view.map(_.path) filterNot changedPaths map RepoChange.Deleted.apply
    (deletedEvents ++ addedOrChanged)
      .toVector
      .sortBy(_.path)
  }

  private def toAddedOrChanged(previousPathToItem: ItemPath => Option[InventoryItem], ignoreVersion: Boolean)(item: InventoryItem): Option[RepoChange] =
    previousPathToItem(item.path) match {
      case Some(previous) if previous == (if (ignoreVersion) item.withVersion(previous.id.versionId) else item) =>
        None

      case Some(_) =>
        Some(RepoChange.Updated(item))

      case None =>
        Some(RepoChange.Added(item))
    }

  final case class Diff[P <: ItemPath, A <: InventoryItem](added: Seq[A] = Nil, updated: Seq[A] = Nil, deleted: Seq[P] = Nil)
  {
    /** For tests: ordering is irrelevant. */
    override def equals(other: Any) = other match {
      case o: Diff[_, _] => added.toSet == o.added.toSet && updated.toSet == o.updated.toSet && deleted.toSet == deleted.toSet
      case _ => false
    }

    def isEmpty = added.isEmpty && updated.isEmpty && deleted.isEmpty

    /** Returns a subset of a certain `ItemPath` and `InventoryItem`. */
    def select[P1 <: P, A1 <: A](implicit A1: InventoryItem.Companion[A1]): Diff[P1, A1] =
      Diff(
        added   collect { case o if o.companion eq A1 => o.asInstanceOf[A1] },
        updated collect { case o if o.companion eq A1 => o.asInstanceOf[A1] },
        deleted collect { case o if o.companion eq A1.itemPathCompanion => o.asInstanceOf[P1] })

    def withVersionId(versionId: VersionId): Diff[P, A] = copy(
      added = added.map(_.withVersion(versionId).asInstanceOf[A]),
      updated = updated.map(_.withVersion(versionId).asInstanceOf[A]))
  }
  object Diff {
    def fromRepoChanges(events: Seq[RepoChange]) =
      Diff[ItemPath, InventoryItem](
        events collect { case o: RepoChange.Added => o.item },
        events collect { case o: RepoChange.Updated => o.item },
        events collect { case o: RepoChange.Deleted => o.path })
  }
}
