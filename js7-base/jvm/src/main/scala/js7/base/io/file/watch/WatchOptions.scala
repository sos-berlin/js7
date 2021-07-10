package js7.base.io.file.watch

import java.nio.file.{Path, WatchEvent}
import js7.base.time.ScalaTime._
import scala.concurrent.duration.FiniteDuration

final case class WatchOptions(
  directory: Path,
  kinds: Set[WatchEvent.Kind[Path]],
  matches: Path => Boolean,
  retryDelays: Seq[FiniteDuration],
  pollTimeout: FiniteDuration,
  delay: FiniteDuration = ZeroDuration)

object WatchOptions
{
  def forTest(  directory: Path,
    kinds: Set[WatchEvent.Kind[Path]],
    fileMatches: Path => Boolean = _ => true,
    retryDurations: Seq[FiniteDuration] = Seq(100.ms),
    pollTimeout: FiniteDuration = 99.s,
    delay: FiniteDuration = ZeroDuration)
  = WatchOptions(directory, kinds, fileMatches, retryDurations, pollTimeout, delay)
}
