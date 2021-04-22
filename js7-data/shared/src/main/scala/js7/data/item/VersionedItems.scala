package js7.data.item

/**
  * @author Joacim Zschimmer
  */
object VersionedItems
{
  def diffVersionedItems(changed: Iterable[VersionedItem], base: Iterable[VersionedItem], ignoreVersion: Boolean = false): Seq[RepoChange] = {
    val pathToMaybeItem = base.view.map(o => o.path -> o).toMap[ItemPath, VersionedItem].lift
    val addedOrChanged = changed.view flatMap toAddedOrChanged(pathToMaybeItem, ignoreVersion)
    val changedPaths = changed.view.map(_.path).toSet
    val deletedEvents = base.view.map(_.path) filterNot changedPaths map RepoChange.Deleted.apply
    (deletedEvents ++ addedOrChanged)
      .toVector
      .sortBy(_.path)
  }

  private def toAddedOrChanged(previousPathToItem: ItemPath => Option[VersionedItem], ignoreVersion: Boolean)(item: VersionedItem): Option[RepoChange] =
    previousPathToItem(item.path) match {
      case Some(previous) if previous == (if (ignoreVersion) item.withVersion(previous.key.versionId) else item) =>
        None

      case Some(_) =>
        Some(RepoChange.Changed(item))

      case None =>
        Some(RepoChange.Added(item))
    }

  final case class Diff[P <: ItemPath, A <: VersionedItem](added: Seq[A] = Nil, changed: Seq[A] = Nil, deleted: Seq[P] = Nil)
  {
    /** For tests: ordering is irrelevant. */
    override def equals(other: Any) = other match {
      case o: Diff[_, _] => added.toSet == o.added.toSet && changed.toSet == o.changed.toSet && deleted.toSet == deleted.toSet
      case _ => false
    }

    def isEmpty = added.isEmpty && changed.isEmpty && deleted.isEmpty

    /** Returns a subset of a certain `ItemPath` and `VersionedItem`. */
    def select[P1 <: P, A1 <: A](implicit A1: VersionedItem.Companion[A1]): Diff[P1, A1] =
      Diff(
        added   collect { case o if o.companion eq A1 => o.asInstanceOf[A1] },
        changed collect { case o if o.companion eq A1 => o.asInstanceOf[A1] },
        deleted collect { case o if o.companion eq A1.Path => o.asInstanceOf[P1] })

    def withVersionId(versionId: VersionId): Diff[P, A] = copy(
      added = added.map(_.withVersion(versionId).asInstanceOf[A]),
      changed = changed.map(_.withVersion(versionId).asInstanceOf[A]))
  }
  object Diff {
    def fromRepoChanges(events: Seq[RepoChange]) =
      Diff[ItemPath, VersionedItem](
        events collect { case o: RepoChange.Added => o.item },
        events collect { case o: RepoChange.Changed => o.item },
        events collect { case o: RepoChange.Deleted => o.path })
  }
}
