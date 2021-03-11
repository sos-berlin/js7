package js7.agent.data.ordersource

import java.nio.file.{Path, Paths}
import js7.agent.data.ordersource.FileOrderSourceState._
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.JavaJsonCodecs.PathJsonCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.file.watch.DirectoryState
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SetOnce
import js7.data.ordersource.OrderSourceEvent.{OrderSourceAgentEvent, OrderSourceOrderArised, OrderSourceOrderVanished}
import js7.data.ordersource.{FileOrderSource, OrderSourceId, SourceOrderName}
import monix.reactive.Observable
import scala.collection.mutable

final case class FileOrderSourceState(
  fileOrderSource: FileOrderSource,
  directoryState: DirectoryState)
{
  def id = fileOrderSource.id

  def applyEvent(event: OrderSourceAgentEvent): FileOrderSourceState =
    event match {
      case OrderSourceOrderArised(SourceOrderName(relativePath_), arguments) =>
        val relativePath = Paths.get(relativePath_)
        copy(
          directoryState =
            directoryState.copy(
              pathToEntry = directoryState.pathToEntry +
                (relativePath -> DirectoryState.Entry(relativePath))))

      case OrderSourceOrderVanished(SourceOrderName(relativePath_)) =>
        val relativePath = Paths.get(relativePath_)
        copy(
          directoryState =
            directoryState.copy(
              pathToEntry = directoryState.pathToEntry - relativePath))
    }

  def estimatedSnapshotSize =
    1 + directoryState.pathToEntry.size

  def toSnapshot: Observable[Snapshot] =
    Observable.pure(HeaderSnapshot(fileOrderSource)) ++
      Observable.fromIterable(directoryState.pathToEntry.values)
        .map(entry => EntrySnapshot(id, entry.path))
}

object FileOrderSourceState
{
  sealed trait Snapshot {
    def orderSourceId: OrderSourceId
  }

  final case class HeaderSnapshot(orderSource: FileOrderSource)
  extends Snapshot {
    def orderSourceId = orderSource.id
  }

  final case class EntrySnapshot(
    orderSourceId: OrderSourceId,
    path: Path)
  extends Snapshot

  implicit val jsonCodec = TypedJsonCodec[Snapshot](
    Subtype.named(deriveCodec[HeaderSnapshot], "FileOrderSource.Header"),
    Subtype.named(deriveCodec[EntrySnapshot], "FileOrderSource.File"))

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

    def result() = FileOrderSourceState(
      header.get.orderSource,
      DirectoryState.fromIterable(entries))
  }

  intelliJuseImport(PathJsonCodec)
}
