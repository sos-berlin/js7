package js7.base.io.file.watch

import java.nio.file.{Path, WatchEvent}
import scala.concurrent.duration.{Duration, FiniteDuration}

final case class WatchOptions(
  directory: Path,
  kinds: Set[WatchEvent.Kind[Path]],
  pollDuration: FiniteDuration,
  delay: FiniteDuration = Duration.Zero)
