package js7.base.io.file.watch

import cats.data.NonEmptySeq
import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.configutils.Configs.*
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.time.JavaTimeConverters.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

final case class DirectoryWatchSettings(
  watchDelay: FiniteDuration,
  pollTimeout: FiniteDuration,
  retryDelays: NonEmptySeq[FiniteDuration],
  directorySilence: FiniteDuration,
  logDelays: NonEmptySeq[FiniteDuration]) {

  def toWatchOptions(directory: Path, isRelevantFile: Path => Boolean): WatchOptions =
    WatchOptions(
      directory,
      isRelevantFile = isRelevantFile,
      watchDelay = watchDelay,
      pollTimeout = pollTimeout,
      retryDelays = retryDelays)
}

object DirectoryWatchSettings {
  def fromConfig(config: Config): Checked[DirectoryWatchSettings] =
    for
      watchDelay <- config.finiteDuration("js7.directory-watch.watch-delay")
      pollTimeout <- config.finiteDuration("js7.directory-watch.poll-timeout")
      retryDelays <- catchNonFatal(NonEmptySeq
        .fromSeq(config
          .getDurationList("js7.directory-watch.retry-delays")
          .asScala.map(_.toFiniteDuration).toVector)
        .getOrElse(NonEmptySeq.one(10.s)))
      directorySilence <- config.finiteDuration("js7.directory-watch.directory-silence")
      logDelays <- catchNonFatal(NonEmptySeq
        .fromSeq(config
          .getDurationList("js7.directory-watch.log-delays")
          .asScala.map(_.toFiniteDuration).toVector)
        .getOrElse(NonEmptySeq.one(10.s)))
    yield
      DirectoryWatchSettings(watchDelay, pollTimeout, retryDelays, directorySilence, logDelays)

  def forTest(pollTimeout: FiniteDuration = 60.s): DirectoryWatchSettings =
    DirectoryWatchSettings(
      watchDelay = 0.s,
      pollTimeout = pollTimeout,
      retryDelays = NonEmptySeq.one(100.ms),
      directorySilence = 100.ms,
      logDelays = NonEmptySeq.of(0.s, 10.s))
}
