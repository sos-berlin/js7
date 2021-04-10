package js7.base.io.file.watch

import io.circe.{Decoder, Encoder, Json, JsonObject}
import java.nio.file.{Files, Path, Paths}
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryState._
import js7.base.log.Logger
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Collections.implicits._
import js7.base.utils.JavaCollections.syntax._
import js7.base.utils.MapDiff
import js7.base.utils.ScalaUtils.syntax._
import scala.collection.{View, mutable}
import scala.concurrent.duration.Deadline.now

final case class DirectoryState(pathToEntry: Map[Path, Entry])
{
  def applyAndReduceEvents(events: Seq[DirectoryEvent]): (Seq[DirectoryEvent], DirectoryState) = {
    val added = mutable.Map.empty[Path, Entry]
    val deleted = mutable.Set.empty[Path]
    val modified = mutable.Set.empty[Path]

    events foreach {
      case FileAdded(path) =>
        added += path -> Entry(path)
        deleted -= path
        modified -= path

      case FileModified(path) =>
        modified += path

      case FileDeleted(path) =>
        added -= path
        deleted += path
        modified -= path
    }

    val updatedState = copy(pathToEntry -- deleted ++ added)
    val reducedEvents = diffTo(updatedState) ++
      modified.filter(updatedState.pathToEntry.keySet).map(FileModified)
    reducedEvents -> updatedState
  }

  def diffTo(other: DirectoryState): Seq[DirectoryEvent] =
    diffToDirectoryEvents(MapDiff.diff(pathToEntry, other.pathToEntry))
      .toVector

  def isEmpty =
    pathToEntry.isEmpty
}

object DirectoryState
{
  val empty = new DirectoryState(Map.empty)
  private val logger = Logger[this.type]

  def fromIterable(entries: Iterable[Entry]): DirectoryState =
    new DirectoryState(entries.toKeyedMap(_.path))

  def readDirectory(directory: Path, matches: Path => Boolean = _ => true): DirectoryState = {
    val since = now
    val directoryState = DirectoryState(
      autoClosing(Files.list(directory))(_
        .asScala
        .flatMap(file =>
          file.startsWith(directory) ?  // Ignore silently alien paths
            directory.relativize(file))
        .filter { relativePath =>
          val s = relativePath.toString
          s != "." && s != ".." && matches(relativePath)
        }
        .map(path => path -> DirectoryState.Entry(path))
        .toMap))
    logger.debug(s"readDirectory '$directory' => ${directoryState.pathToEntry.size} files in ${since.elapsed.pretty}")
    directoryState
  }

  final case class Entry(path: Path)

  private def diffToDirectoryEvents(diff: MapDiff[Path, Entry]): View[DirectoryEvent] =
    diff.deleted.view.map(FileDeleted) ++
      diff.updated.keys.view.map(FileModified) ++
      diff.added.keySet.view.map(FileAdded)

  implicit val jsonEncoder: Encoder.AsObject[DirectoryState] =
    o => JsonObject.fromIterable(o
      .pathToEntry
      .values
      .view
      .map(e => e.path.toString -> Json.fromJsonObject(JsonObject.empty)))

  implicit val jsonDecoder: Decoder[DirectoryState] =
    c => for {
      jsonObject <- c.as[JsonObject]
      entries <- Right(jsonObject.toIterable.map { case (k, v) => Entry(Paths.get(k)) })
    } yield DirectoryState.fromIterable(entries)
}
