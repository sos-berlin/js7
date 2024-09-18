package js7.agent.data.orderwatch

import cats.effect.IO
import fs2.Stream
import io.circe.generic.semiauto.deriveCodec
import java.nio.file.{Path, Paths}
import js7.agent.data.orderwatch.FileWatchState.{EntrySnapshot, HeaderSnapshot, ItemState, Snapshot}
import js7.base.circeutils.JavaFileJsonCodecs.PathJsonCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SetOnce
import js7.data.event.KeyedEvent
import js7.data.item.UnsignedSimpleItemState
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchEvent, OrderWatchPath}
import scala.collection.{View, mutable}

final case class FileWatchState(
  fileWatch: FileWatch,
  directoryState: DirectoryState)
extends UnsignedSimpleItemState:

  protected type Self = FileWatchState
  val companion: FileWatchState.type = FileWatchState

  val item: FileWatch = fileWatch
  def path: OrderWatchPath = item.path

  def updateItem(item: FileWatch): Checked[ItemState] =
    Right(copy(fileWatch = item))

  def id: OrderWatchPath = fileWatch.path

  def applyEvent(event: OrderWatchEvent): FileWatchState =
    event match
      case ExternalOrderArised(ExternalOrderName(filename_), _, _) =>
        val filename = Paths.get(filename_)
        copy(
          directoryState =
            directoryState.copy(
              fileToEntry = directoryState.fileToEntry +
                (filename -> DirectoryState.Entry(filename))))

      case ExternalOrderVanished(ExternalOrderName(filename_)) =>
        val filename = Paths.get(filename_)
        copy(
          directoryState =
            directoryState.copy(
              fileToEntry = directoryState.fileToEntry - filename))

  def containsPath(path: Path): Boolean =
    directoryState.fileToEntry.contains(path)

  def allFilesVanished: View[KeyedEvent[ExternalOrderVanished]] =
    directoryState.fileToEntry.keys
      .view
      .map(file =>
        fileWatch.path <-: ExternalOrderVanished(ExternalOrderName(file.toString)))

  def estimatedExtraSnapshotSize: Int =
    directoryState.fileToEntry.size

  override def toSnapshotStream: Stream[IO, Snapshot] =
    Stream.emit(HeaderSnapshot(fileWatch)) ++
      Stream.iterable(directoryState.fileToEntry.values)
        .map(entry => EntrySnapshot(id, entry.path))


object FileWatchState extends UnsignedSimpleItemState.Companion[FileWatchState]:
  type Key = OrderWatchPath
  type Item = FileWatch
  override type ItemState = FileWatchState

  sealed trait Snapshot:
    def orderWatchPath: OrderWatchPath

  final case class HeaderSnapshot(fileWatch: FileWatch)
  extends Snapshot:
    def orderWatchPath: OrderWatchPath = fileWatch.path
    override def productPrefix = "FileWatchState"

  final case class EntrySnapshot(
    orderWatchPath: OrderWatchPath,
    path: java.nio.file.Path)
  extends Snapshot:
    override def productPrefix = "FileWatchState.File"

  implicit val jsonCodec: TypedJsonCodec[Snapshot] = TypedJsonCodec(
    Subtype.named(deriveCodec[HeaderSnapshot], "FileWatchState"),
    Subtype.named(deriveCodec[EntrySnapshot], "FileWatchState.File"))

  final class Builder:
    private val header = SetOnce[HeaderSnapshot]
    private val entries = mutable.Buffer.empty[DirectoryState.Entry]

    def addSnapshot(snapshot: Snapshot): Unit =
      snapshot match
        case o: HeaderSnapshot =>
          header := o
        case o: EntrySnapshot =>
          entries += DirectoryState.Entry(o.path)

    def result(): FileWatchState =
      FileWatchState(
        header.orThrow.fileWatch,
        DirectoryState.fromIterable(entries))

  intelliJuseImport(PathJsonCodec)
