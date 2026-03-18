package js7.base.io.file.watch

import java.nio.file.Path
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryState.*
import js7.base.utils.SetDiff
import scala.collection.{View, mutable}

final case class DirectoryState(files: Set[Path]):

  def applyAndReduceEvents(events: Seq[DirectoryEvent]): (Seq[DirectoryEvent], DirectoryState) =
    val added = mutable.Set.empty[Path]
    val deleted = mutable.Set.empty[Path]
    val modified = mutable.Set.empty[Path]

    events foreach:
      case FileAdded(path) =>
        added += path
        deleted -= path
        modified -= path

      case FileModified(path) =>
        modified += path

      case FileDeleted(path) =>
        added -= path
        deleted += path
        modified -= path

    val updatedState = copy(files -- deleted ++ added)
    val reducedEvents = diffTo(updatedState) ++
      modified.filter(updatedState.files).map(FileModified(_))
    reducedEvents -> updatedState

  def diffTo(other: DirectoryState): Seq[DirectoryEvent] =
    diffToDirectoryEvents(SetDiff.diff(files, other.files))
      .toVector

  def isEmpty: Boolean =
    files.isEmpty


object DirectoryState:
  val empty: DirectoryState =
    new DirectoryState(Set.empty)

  def apply(entries: Iterable[Path]): DirectoryState =
    new DirectoryState(entries.toSet)

  private def diffToDirectoryEvents(diff: SetDiff[Path]): View[DirectoryEvent] =
    diff.deleted.view.map(FileDeleted(_)) ++
      diff.added.view.map(FileAdded(_))
