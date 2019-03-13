package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.filebased.{FileBased, RepoChange, TypedPath, VersionId}
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
object FileBaseds
{
  def diffFileBaseds(changed: Iterable[FileBased], base: Iterable[FileBased], ignoreVersion: Boolean = false): Seq[RepoChange] =
    diffFileBaseds2(changed, base, ignoreVersion).sortBy(_.path)

  private def diffFileBaseds2(changed: Iterable[FileBased], base: Iterable[FileBased], ignoreVersion: Boolean): Seq[RepoChange] = {
    val pathToFileBased = base toKeyedMap (_.path: TypedPath)
    val addedOrChanged = changed.toVector flatMap toAddedOrChanged(pathToFileBased.lift, ignoreVersion)
    val readPaths = changed.map(_.path).toSet
    val deletedEvents = base map (_.path) filterNot readPaths map RepoChange.Deleted.apply
    deletedEvents.toVector ++ addedOrChanged
  }

  private def toAddedOrChanged(previousPathToFileBased: TypedPath => Option[FileBased], ignoreVersion: Boolean)(fileBased: FileBased): Option[RepoChange] =
    previousPathToFileBased(fileBased.path) match {
      case Some(previous) if previous == (if (ignoreVersion) fileBased.withVersion(previous.id.versionId) else fileBased) =>
        None

      case Some(_) =>
        Some(RepoChange.Updated(fileBased))

      case None =>
        Some(RepoChange.Added(fileBased))
    }

  final case class Diff[P <: TypedPath, A <: FileBased](added: Seq[A] = Nil, updated: Seq[A] = Nil, deleted: Seq[P] = Nil)
  {
    /** For tests: ordering is irrelevant. */
    override def equals(other: Any) = other match {
      case o: Diff[_, _] => added.toSet == o.added.toSet && updated.toSet == o.updated.toSet && deleted.toSet == deleted.toSet
      case _ => false
    }

    def isEmpty = added.isEmpty && updated.isEmpty && deleted.isEmpty

    /** Returns a subset of a certain `TypedPath` and `FileBased`. */
    def select[P1 <: P, A1 <: A](implicit A1Path: TypedPath.Companion[P1], A1: FileBased.Companion[A1]): Diff[P1, A1] =
      Diff(
        added   collect { case o if o.companion eq A1 => o.asInstanceOf[A1] },
        updated collect { case o if o.companion eq A1 => o.asInstanceOf[A1] },
        deleted collect { case o if o.companion eq A1.typedPathCompanion => o.asInstanceOf[P1] })

    def withVersionId(versionId: VersionId): Diff[P, A] = copy(
      added = added map (_.withVersion(versionId).asInstanceOf[A]),
      updated = updated map (_.withVersion(versionId).asInstanceOf[A]))
  }
  object Diff {
    def fromRepoChanges(events: Seq[RepoChange]) =
      Diff[TypedPath, FileBased](
        events collect { case o: RepoChange.Added => o.fileBased },
        events collect { case o: RepoChange.Updated => o.fileBased },
        events collect { case o: RepoChange.Deleted => o.path })
  }
}
