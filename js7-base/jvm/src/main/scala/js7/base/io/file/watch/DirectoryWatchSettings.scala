package js7.base.io.file.watch

import com.typesafe.config.Config
import java.nio.file.Path
import js7.base.configutils.Configs.*
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.DelayConf
import js7.base.utils.DelayConf.delayConf
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

final case class DirectoryWatchSettings(
  watchDelay: FiniteDuration,
  pollTimeout: FiniteDuration,
  delayConf: DelayConf,
  directorySilence: FiniteDuration,
  logDelayConf: DelayConf):

  def toWatchOptions(directory: Path, isRelevantFile: Path => Boolean): WatchOptions =
    WatchOptions(
      directory,
      isRelevantFile = isRelevantFile,
      watchDelay = watchDelay,
      pollTimeout = pollTimeout,
      delayConf = delayConf)


object DirectoryWatchSettings:
  def fromConfig(config: Config): Checked[DirectoryWatchSettings] =
    for
      watchDelay <- config.finiteDuration("js7.directory-watch.watch-delay")
      pollTimeout <- config.finiteDuration("js7.directory-watch.poll-timeout")
      delayConf <- config.delayConf("js7.directory-watch.retry-delays")
      directorySilence <- config.finiteDuration("js7.directory-watch.directory-silence")
      logDelayConf <- config.delayConf("js7.directory-watch.log-delays")
    yield
      DirectoryWatchSettings(watchDelay, pollTimeout, delayConf, directorySilence, logDelayConf)

  def forTest(pollTimeout: FiniteDuration = 60.s): DirectoryWatchSettings =
    DirectoryWatchSettings(
      watchDelay = 0.s,
      pollTimeout = pollTimeout,
      delayConf = DelayConf(100.ms),
      directorySilence = 100.ms,
      logDelayConf = DelayConf(0.s, 10.s))
