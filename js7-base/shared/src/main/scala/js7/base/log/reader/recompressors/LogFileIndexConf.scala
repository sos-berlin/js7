package js7.base.log.reader.recompressors

import com.typesafe.config.Config
import js7.base.config.Js7Config
import js7.base.configutils.Configs.RichConfig
import js7.base.io.file.watch.DirectoryWatchSettings
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.utils.DelayConf
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration

final case class LogFileIndexConf(
  fileAddedDelay: FiniteDuration,
  currentFileMaxDelay: FiniteDuration,
  timestampReaderConcurrency: Int,
  logFileTimestampTries: DelayConf,
  pollGrowing: FiniteDuration,
  directoryWatchSettings: DirectoryWatchSettings,
  recompressor: Recompressor)

object LogFileIndexConf:
  def fromConfig(config: Config): Checked[LogFileIndexConf] =
    for
      fileAddedDelay <- config.finiteDuration("js7.log.file-added-delay")
      currentFileMaxDelay <- config.finiteDuration("js7.log.current-file-max-delay")
      concurrency <- catchNonFatal(config.getInt("js7-log.read-timestamp-concurrency"))
      logFileTimestampTries <- DelayConf.fromConfig(config, "js7.log.read-timestamp-tries")
      pollGrowing <- config.finiteDuration("js7.log.poll-growing")
      directoryWatchSettings <- DirectoryWatchSettings.fromConfig(config)
      recompressor = Recompressor.fromConfig(config)
    yield
      LogFileIndexConf(fileAddedDelay, currentFileMaxDelay,
        concurrency, logFileTimestampTries, pollGrowing, directoryWatchSettings, recompressor)

  lazy val forTest: LogFileIndexConf =
    LogFileIndexConf.fromConfig(Js7Config.defaultConfig).orThrow
