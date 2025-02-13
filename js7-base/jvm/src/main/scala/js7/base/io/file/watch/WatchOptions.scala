package js7.base.io.file.watch

import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import java.nio.file.{Path, WatchEvent}
import js7.base.time.ScalaTime.*
import js7.base.utils.DelayConf
import scala.concurrent.duration.FiniteDuration

final case class WatchOptions(
  directory: Path,
  kinds: Set[WatchEvent.Kind[Path]] = defaultKinds,
  isRelevantFile: Path => Boolean = everyFileIsRelevant,
  watchDelay: FiniteDuration = ZeroDuration,
  pollTimeout: FiniteDuration,
  delayConf: DelayConf)


object WatchOptions:

  private val defaultKinds: Set[WatchEvent.Kind[Path]] =
    Set(ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE)

  val everyFileIsRelevant: Path => Boolean =
    _ => true

  def forTest(
    directory: Path,
    kinds: Set[WatchEvent.Kind[Path]] = defaultKinds,
    isRelevantFile: Path => Boolean = everyFileIsRelevant,
    watchDelay: FiniteDuration = ZeroDuration,
    pollTimeout: FiniteDuration = 999.s,
    delayConf: DelayConf = DelayConf(100.ms))
  : WatchOptions
  = WatchOptions(directory, kinds, isRelevantFile, watchDelay, pollTimeout, delayConf)
