package js7.base.io.file.watch

import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW}
import java.nio.file.{Path, WatchEvent}
import js7.base.io.file.watch.DirectoryEvent._

sealed trait DirectoryWatchEvent

object DirectoryWatchEvent
{
  private val PathOverflow = OVERFLOW.asInstanceOf[WatchEvent.Kind[Path]]

  def fromJava(watchEvent: WatchEvent[Path]): DirectoryWatchEvent =
    watchEvent.kind match {
      case PathOverflow => DirectoryWatchOverflow
      case ENTRY_CREATE => FileAdded(watchEvent.context)
      case ENTRY_DELETE => FileDeleted(watchEvent.context)
      case ENTRY_MODIFY => FileModified(watchEvent.context)
    }

  final case object DirectoryWatchOverflow extends DirectoryWatchEvent
}

sealed trait DirectoryEvent extends DirectoryWatchEvent {
  def relativePath: Path
}

object DirectoryEvent
{
  final case class FileAdded(relativePath: Path) extends DirectoryEvent
  final case class FileDeleted(relativePath: Path) extends DirectoryEvent
  final case class FileModified(relativePath: Path) extends DirectoryEvent
}
