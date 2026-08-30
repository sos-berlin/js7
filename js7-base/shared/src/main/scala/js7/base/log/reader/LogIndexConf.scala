package js7.base.log.reader

import com.typesafe.config.Config
import js7.base.config.Js7Config
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.ByteChunksLineSplitter.MinimumLength
import js7.base.io.file.watch.DirectoryWatchSettings
import js7.base.log.reader.recompressors.Recompressor
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.utils.DelayConf
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration

final case class LogIndexConf(
  fileAddedDelay: FiniteDuration,
  currentFileMaxDelay: FiniteDuration,
  timestampReaderConcurrency: Int,
  logFileTimestampTries: DelayConf,
  logFileIndexLineLength: Int,
  pollGrowing: FiniteDuration,
  directoryWatchSettings: DirectoryWatchSettings,
  recompressor: Recompressor)

object LogIndexConf:
  def fromConfig(config: Config): Checked[LogIndexConf] =
    for
      fileAddedDelay <- config.finiteDuration("js7.log.file-added-delay")
      currentFileMaxDelay <- config.finiteDuration("js7.log.current-file-max-delay")
      concurrency <- catchNonFatal(config.getInt("js7-log.read-timestamp-concurrency"))
      logFileTimestampTries <- DelayConf.fromConfig(config, "js7.log.read-timestamp-tries")
      logFileIndexLineLength <-
        catchNonFatal:
          val n = config.getBytes("js7.log.index.max-bytes-per-line")
          if n <= MinimumLength || n > Int.MaxValue then
            throw new IllegalArgumentException(
              s"js7.log.index.max-bytes-per-line must be > $MinimumLength and <= ${Int.MaxValue}")
          n.toInt
      pollGrowing <- config.finiteDuration("js7.log.poll-growing")
      directoryWatchSettings <- DirectoryWatchSettings.fromConfig(config)
      recompressor = Recompressor.fromConfig(config)
    yield
      LogIndexConf(fileAddedDelay, currentFileMaxDelay,
        concurrency, logFileTimestampTries, logFileIndexLineLength, pollGrowing,
        directoryWatchSettings, recompressor)

  val default: LogIndexConf =
    LogIndexConf.fromConfig(Js7Config.defaultConfig).orThrow

  lazy val forTest: LogIndexConf =
    default
