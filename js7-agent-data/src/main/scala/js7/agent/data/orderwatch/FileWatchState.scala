package js7.agent.data.orderwatch

import io.circe.generic.semiauto.deriveCodec
import java.nio.file.{Path, Paths}
import js7.agent.data.orderwatch.FileWatchState.{EntrySnapshot, HeaderSnapshot, Snapshot}
import js7.base.circeutils.JavaJsonCodecs.PathJsonCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.file.watch.DirectoryState
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SetOnce
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemState
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import monix.reactive.Observable
import scala.collection.{View, mutable}

final case class FileWatchState(
  fileWatch: FileWatch,
  directoryState: DirectoryState)
extends UnsignedSimpleItemState
{
  protected type Self = FileWatchState
  val companion = FileWatchState

  val item = fileWatch
  def path = item.path

  def updateItem(item: FileWatch) =
    Right(copy(fileWatch = item))

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

  def allFilesVanished: View[KeyedEvent[ExternalOrderVanished]] =
    directoryState.fileToEntry.keys
      .view
      .map(file =>
        fileWatch.path <-: ExternalOrderVanished(ExternalOrderName(file.toString)))

  def estimatedExtraSnapshotSize =
    directoryState.fileToEntry.size

  override def toSnapshotObservable: Observable[Snapshot] =
    Observable.pure(HeaderSnapshot(fileWatch)) ++
      Observable.fromIterable(directoryState.fileToEntry.values)
        .map(entry => EntrySnapshot(id, entry.path))
}

object FileWatchState extends UnsignedSimpleItemState.Companion[FileWatchState]
{
  type Key = OrderWatchPath
  type Item = FileWatch
  override type ItemState = FileWatchState

  sealed trait Snapshot {
    def orderWatchPath: OrderWatchPath
  }

  final case class HeaderSnapshot(fileWatch: FileWatch)
  extends Snapshot {
    def orderWatchPath = fileWatch.path
    override def productPrefix = "FileWatchState"
  }

  final case class EntrySnapshot(
    orderWatchPath: OrderWatchPath,
    path: java.nio.file.Path)
  extends Snapshot {
    override def productPrefix = "FileWatchState.File"
  }

  implicit val jsonCodec: TypedJsonCodec[Snapshot] = TypedJsonCodec(
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
