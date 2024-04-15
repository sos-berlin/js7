package js7.base.io.file.watch

import java.nio.file.Path
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryState.*
import js7.base.utils.Collections.implicits.*
import js7.base.utils.MapDiff
import scala.collection.{View, mutable}

final case class DirectoryState(fileToEntry: Map[Path, Entry]):

  def applyAndReduceEvents(events: Seq[DirectoryEvent]): (Seq[DirectoryEvent], DirectoryState) =
    val added = mutable.Map.empty[Path, Entry]
    val deleted = mutable.Set.empty[Path]
    val modified = mutable.Set.empty[Path]

    events foreach:
      case FileAdded(path) =>
        added.update(path, Entry(path))
        deleted -= path
        modified -= path

      case FileModified(path) =>
        modified += path

      case FileDeleted(path) =>
        added -= path
        deleted += path
        modified -= path

    val updatedState = copy(fileToEntry -- deleted ++ added)
    val reducedEvents = diffTo(updatedState) ++
      modified.filter(updatedState.fileToEntry.keySet).map(FileModified(_))
    reducedEvents -> updatedState

  def diffTo(other: DirectoryState): Seq[DirectoryEvent] =
    diffToDirectoryEvents(MapDiff.diff(fileToEntry, other.fileToEntry))
      .toVector

  def files: Iterable[Path] =
    fileToEntry.keys

  def isEmpty: Boolean =
    fileToEntry.isEmpty


object DirectoryState:
  val empty = new DirectoryState(Map.empty)

  def fromIterable(entries: Iterable[Entry]): DirectoryState =
    new DirectoryState(entries.toKeyedMap(_.path))

  final case class Entry(path: Path)

  private def diffToDirectoryEvents(diff: MapDiff[Path, Entry]): View[DirectoryEvent] =
    diff.deleted.view.map(FileDeleted(_)) ++
      diff.updated.keys.view.map(FileModified(_)) ++
      diff.added.keySet.view.map(FileAdded(_))
