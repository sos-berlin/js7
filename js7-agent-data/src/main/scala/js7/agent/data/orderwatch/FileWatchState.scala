package js7.agent.data.orderwatch

import java.nio.file.{Path, Paths}
import js7.agent.data.orderwatch.FileWatchState._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.JavaJsonCodecs.PathJsonCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.file.watch.DirectoryState
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SetOnce
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import monix.reactive.Observable
import scala.collection.mutable

final case class FileWatchState(
  fileWatch: FileWatch,
  directoryState: DirectoryState)
{
  def id = fileWatch.path

  def applyEvent(event: OrderWatchEvent): FileWatchState =
    event match {
      case ExternalOrderArised(ExternalOrderName(relativePath_), _, _) =>
        val relativePath = Paths.get(relativePath_)
        copy(
          directoryState =
            directoryState.copy(
              fileToEntry = directoryState.fileToEntry +
                (relativePath -> DirectoryState.Entry(relativePath))))

      case ExternalOrderVanished(ExternalOrderName(relativePath_)) =>
        val relativePath = Paths.get(relativePath_)
        copy(
          directoryState =
            directoryState.copy(
              fileToEntry = directoryState.fileToEntry - relativePath))
    }

  def containsPath(path: Path) =
    directoryState.fileToEntry.contains(path)

  def estimatedSnapshotSize =
    1 + directoryState.fileToEntry.size

  def toSnapshot: Observable[Snapshot] =
    Observable.pure(HeaderSnapshot(fileWatch)) ++
      Observable.fromIterable(directoryState.fileToEntry.values)
        .map(entry => EntrySnapshot(id, entry.path))
}

object FileWatchState
{
  sealed trait Snapshot {
    def orderWatchPath: OrderWatchPath
  }

  final case class HeaderSnapshot(fileWatch: FileWatch)
  extends Snapshot {
    def orderWatchPath = fileWatch.path
  }

  final case class EntrySnapshot(
    orderWatchPath: OrderWatchPath,
    path: Path)
  extends Snapshot

  implicit val jsonCodec = TypedJsonCodec[Snapshot](
    Subtype.named(deriveCodec[HeaderSnapshot], "FileWatchState"),
    Subtype.named(deriveCodec[EntrySnapshot], "FileWatchState.File"))

  final class Builder {
    private val header = SetOnce[HeaderSnapshot]
    private val entries = mutable.Buffer.empty[DirectoryState.Entry]

    def addSnapshot(snapshot: Snapshot): Unit =
      snapshot match {
        case o: HeaderSnapshot =>
          header := o
        case o: EntrySnapshot =>
          entries += DirectoryState.Entry(o.path)
      }

    def result() = FileWatchState(
      header.orThrow.fileWatch,
      DirectoryState.fromIterable(entries))
  }

  intelliJuseImport(PathJsonCodec)
}
