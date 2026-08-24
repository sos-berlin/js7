package js7.base.log.reader

import cats.effect.{IO, SyncIO}
import java.nio.file.{Files, Path, Paths}
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.io.file.FileDeleter
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.LogLevel
import scala.util.Try
import scala.util.matching.Regex

private object LogUtils:

  val LogLevels = Set(LogLevel.Error, LogLevel.Info, LogLevel.Debug)
  private[reader] val TmpSuffix = "-indexed.tmp"
  private[reader] val LogGzTmpSuffix = ".log.gz" + TmpSuffix

  private val NameRegex =
    s"^([^-.]+)(-error|-debug)?(?:\\.log|(?:-.+)?\\.log\\.gz)(?:${Regex.quote(TmpSuffix)})?$$".r

  def deleteTmpFiles(directory: Path, logFilePrefixes: Set[String]): IO[Unit] =
    IO.blocking:
      if Files.exists(directory) then // In test situations, the logs directory may not exist
        FileDeleter.tryDeleteFiles:
          directory.directoryStream[SyncIO]
            .filter: file =>
              val strippedFilename = Paths.get(file.getFileName.toString.stripSuffix(TmpSuffix))
              isOurTmpFile(file) && isOurLogFilename(logFilePrefixes, strippedFilename)
            .compile.toVector
            .run()

  private[reader] def isOurTmpFile(file: Path): Boolean =
    file.toString.endsWith(LogGzTmpSuffix)

  private[reader] def isOurLogFilename(isValidPrefix: String => Boolean, file: Path): Boolean =
    fileToPrefixAndLogLevel(file) match
      case Some((prefix, _)) => isValidPrefix(prefix)
      case _ => false

  def fileToPrefixAndLogLevel(file: Path): Option[(String, LogLevel)] =
    file.getFileName.toString match
      case NameRegex(prefix: String, logLevel) =>
        logLevel match
          case null => Some(prefix -> LogLevel.Info)
          case logLevel: String =>
            Try(LogLevel(logLevel.substring(1).capitalize)).toOption.map(prefix -> _)
      case _ => None
