package js7.base.log.reader

import com.typesafe.config.Config
import js7.base.config.Js7Config
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.ByteChunksLineSplitter.MinimumLength
import js7.base.io.file.watch.DirectoryWatchSettings
import js7.base.log.Logger
import js7.base.log.reader.LogIndexConf.*
import js7.base.log.reader.recompressors.Recompressor
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.utils.ByteUnits.{toKBGB, toKiBGiB}
import js7.base.utils.DelayConf
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration

final case class LogIndexConf(
  timestampReaderConcurrency: Int,
  recompressor: Recompressor,
  fileAddedDelay: FiniteDuration,
  currentFileMaxDelay: FiniteDuration,
  logFileTimestampTries: DelayConf,
  headerMinimumLength: Int,
  pollGrowing: FiniteDuration,
  fileBufferSize: Int,
  buildBufferSize: Int,
  logBytesPerEntry: Int,
  noEntryWarnThreshold: Int,
  logFileIndexLineLength: Int,
  checkLogFileChangePeriod: FiniteDuration,
  skipBackwardsMinSize: Int,
  skipBackwardsMaxSize: Int,
  directoryWatchSettings: DirectoryWatchSettings):

  val uniqueHeaderSize: Int =
    UniqueHeaderSize


object LogIndexConf:
  private val logger = Logger[LogIndexConf]

  /** Number of first bytes of a log file with a timestamp which should uniquely identify it.
    *
    * The first line of each log file starts with a timestamp including the timezone offset,
    * to uniquely identify it.
    * <p>
    * See log4j2.xml header setting. Some recommended formats:
    * <pre>
    * %d{yyyy-MM-dd HH:mm:ss.SSSX} ...
    * %d{yyyy-MM-dd'T'HH:mm:ss,SSSSSSX} ...
    * </pre>
    */
  private val longestTimestamp = "yyyy-MM-dd HH:mm:ss.SSSSSSSSS+12:34:56"
  private[reader] val UniqueHeaderSize = longestTimestamp.length + 1

  def fromConfig(config: Config): Checked[LogIndexConf] =
    for
      concurrency <- catchNonFatal(config.getInt("js7.log.index.read-timestamp-concurrency"))
      recompressor = Recompressor.fromConfig(config)
      fileAddedDelay <- config.finiteDuration("js7.log.index.file-added-delay")
      currentFileMaxDelay <- config.finiteDuration("js7.log.index.current-file-max-delay")
      logFileTimestampTries <- DelayConf.fromConfig(config, "js7.log.index.read-timestamp-tries")
      headerMinimumLength <- catchNonFatal(config.getInt("js7.log.index.headerMinimumLength"))
      pollGrowing <- config.finiteDuration("js7.log.poll-growing")
      fileBufferSize <- catchNonFatal(config.getBytes("js7.log.index.file-buffer-size").toInt)
      buildBufferSize <- catchNonFatal(config.getBytes("js7.log.index.build-buffer-size").toInt)
      logBytesPerEntry <- catchNonFatal(config.getBytes("js7.log.index.log-bytes-per-entry").toInt)
      noEntryWarnThreshold <- catchNonFatal(config.getBytes("js7.log.index.no-entry-warn-threshold").toInt)
      logFileIndexLineLength <-
        catchNonFatal:
          val n = config.getBytes("js7.log.index.max-bytes-per-line")
          if n <= MinimumLength || n > Int.MaxValue then
            throw new IllegalArgumentException(
              s"js7.log.index.max-bytes-per-line must be > $MinimumLength and <= ${Int.MaxValue}")
          n.toInt
      checkLogFileChangePeriod <- config.finiteDuration("js7.log.index.check-log-file-change-period")
      skipBackwardsMinSize <- catchNonFatal(config.getBytes("js7.log.index.skip-backwards-minimum-size").toInt)
      skipBackwardsMaxSize <- catchNonFatal(config.getBytes("js7.log.index.skip-backwards-maximum-size").toInt)
      directoryWatchSettings <- DirectoryWatchSettings.fromConfig(config)
    yield
      logger.debug(s"Blocksize=${toKiBGiB(logBytesPerEntry)}, requiring ${
        toKBGB(1_000_000_000L * EpochNanoToPos.EntrySize / logBytesPerEntry)
      } memory per gigabyte log file")

      LogIndexConf(
        concurrency,
        recompressor,
        fileAddedDelay, currentFileMaxDelay, logFileTimestampTries, headerMinimumLength,
        pollGrowing,
        fileBufferSize, buildBufferSize, logBytesPerEntry, noEntryWarnThreshold,
        logFileIndexLineLength,
        checkLogFileChangePeriod,
        skipBackwardsMinSize, skipBackwardsMaxSize,
        directoryWatchSettings)

  val default: LogIndexConf =
    LogIndexConf.fromConfig(Js7Config.defaultConfig).orThrow

  lazy val forTest: LogIndexConf =
    default
