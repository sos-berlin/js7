package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.core.filebased.FileBasedReader.readDirectoryTree
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged, FileBasedDeleted, VersionAdded}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath, VersionId}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
object FileBaseds
{
  def readDirectory(
    readers: Iterable[FileBasedReader],
    directory: Path,
    previousFileBaseds: Iterable[FileBased],
    versionId: VersionId,
    ignoreAliens: Boolean = false)
  : Checked[Seq[RepoEvent]] =
    for (fileBaseds ← readDirectoryTree(readers, directory, versionId, ignoreAliens = ignoreAliens)) yield
      VersionAdded(versionId) +: toEvents(fileBaseds, previousFileBaseds)

  private def toEvents(readFileBaseds: Iterable[FileBased], previousFileBaseds: Iterable[FileBased])
  : Seq[RepoEvent] = {
    val pathToFileBased = previousFileBaseds toKeyedMap (_.path: TypedPath)
    val addedOrChangedEvents = readFileBaseds.toVector flatMap toAddedOrChanged(pathToFileBased)
    val readPaths = readFileBaseds.map(_.path).toSet
    val deletedEvents = previousFileBaseds map (_.path) filterNot readPaths map FileBasedDeleted.apply
    deletedEvents.toVector ++ addedOrChangedEvents
  }

  private def toAddedOrChanged(previousPathToFileBased: Map[TypedPath, FileBased])(fileBased: FileBased): Option[RepoEvent] =
    previousPathToFileBased.get(fileBased.path) match {
      case Some(existing) if existing.withVersion(fileBased.id.versionId) == fileBased ⇒
        None

      case Some(_) ⇒
        Some(FileBasedChanged(fileBased))

      case None ⇒
        Some(FileBasedAdded(fileBased))
    }

  final case class Diff[P <: TypedPath, A <: FileBased](added: Seq[A], changed: Seq[A], deleted: Seq[P]) {
    def select[P1 <: P, A1 <: A](implicit A1Path: TypedPath.Companion[P1], A1: FileBased.Companion[A1]): Diff[P1, A1] =
      Diff(
        added   collect { case o if o.companion eq A1 ⇒ o.asInstanceOf[A1] },
        changed collect { case o if o.companion eq A1 ⇒ o.asInstanceOf[A1] },
        deleted collect { case o if o.companion eq A1.typedPathCompanion ⇒ o.asInstanceOf[P1] })
  }
  object Diff {
    def fromEvents(events: Seq[RepoEvent]) =
      Diff[TypedPath, FileBased](
        events collect { case o: FileBasedAdded ⇒ o.fileBased },
        events collect { case o: FileBasedChanged ⇒ o.fileBased },
        events collect { case o: FileBasedDeleted ⇒ o.path })
  }
}
