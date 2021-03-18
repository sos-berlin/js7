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
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchId}
import monix.reactive.Observable
import scala.collection.mutable

final case class FileWatchState(
  fileWatch: FileWatch,
  directoryState: DirectoryState)
{
  def id = fileWatch.id

  def applyEvent(event: OrderWatchEvent): FileWatchState =
    event match {
      case ExternalOrderArised(ExternalOrderName(relativePath_), _, _) =>
        val relativePath = Paths.get(relativePath_)
        copy(
          directoryState =
            directoryState.copy(
              pathToEntry = directoryState.pathToEntry +
                (relativePath -> DirectoryState.Entry(relativePath))))

      case ExternalOrderVanished(ExternalOrderName(relativePath_)) =>
        val relativePath = Paths.get(relativePath_)
        copy(
          directoryState =
            directoryState.copy(
              pathToEntry = directoryState.pathToEntry - relativePath))
    }

  def estimatedSnapshotSize =
    1 + directoryState.pathToEntry.size

  def toSnapshot: Observable[Snapshot] =
    Observable.pure(HeaderSnapshot(fileWatch)) ++
      Observable.fromIterable(directoryState.pathToEntry.values)
        .map(entry => EntrySnapshot(id, entry.path))
}

object FileWatchState
{
  sealed trait Snapshot {
    def orderWatchId: OrderWatchId
  }

  final case class HeaderSnapshot(orderWatch: FileWatch)
  extends Snapshot {
    def orderWatchId = orderWatch.id
  }

  final case class EntrySnapshot(
    orderWatchId: OrderWatchId,
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
      header.get.orderWatch,
      DirectoryState.fromIterable(entries))
  }

  intelliJuseImport(PathJsonCodec)
}
