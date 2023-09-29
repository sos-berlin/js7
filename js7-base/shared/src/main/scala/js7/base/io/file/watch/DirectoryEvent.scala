package js7.base.io.file.watch

import cats.Eq
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW}
import java.nio.file.{Path, WatchEvent}
import js7.base.io.file.watch.DirectoryEvent.*

sealed trait DirectoryWatchEvent

object DirectoryWatchEvent:
  private val PathOverflow = OVERFLOW.asInstanceOf[WatchEvent.Kind[Path]]

  def fromJava(watchEvent: WatchEvent[Path]): DirectoryWatchEvent =
    watchEvent.kind match
      case PathOverflow => Overflow
      case ENTRY_CREATE => FileAdded(watchEvent.context)
      case ENTRY_MODIFY => FileModified(watchEvent.context)
      case ENTRY_DELETE => FileDeleted(watchEvent.context)

  case object Overflow extends DirectoryWatchEvent

sealed trait DirectoryEvent extends DirectoryWatchEvent:
  def relativePath: Path

object DirectoryEvent:
  sealed trait FileAddedOrModified extends DirectoryEvent
  final case class FileAdded(relativePath: Path) extends FileAddedOrModified
  final case class FileModified(relativePath: Path) extends FileAddedOrModified
  final case class FileDeleted(relativePath: Path) extends DirectoryEvent

  implicit val DirectoryEventEq: Eq[DirectoryEvent] =
    Eq.fromUniversalEquals
