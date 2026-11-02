package js7.base.log.reader

import cats.effect.{IO, SyncIO}
import java.nio.file.{Files, Path, Paths}
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.io.file.FileDeleter
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.LogLevel
import js7.base.utils.JavaUtils.-->
import scala.jdk.FunctionConverters.*
import scala.util.Try
import scala.util.matching.Regex
import scala.util.matching.Regex.quote

object LogUtils:

  val LogLevels: Set[LogLevel] = Set(LogLevel.Error, LogLevel.Info, LogLevel.Debug)
  val TmpSuffix: String = "-indexed.tmp"
  val LogGzTmpSuffix: String = ".log.gz" + TmpSuffix


  val NameRegex: Regex =
    // A prefix word, but not "debug" or "error", which denote the log level
    val Word = "(?!(?i:debug|error)(?![0-9A-Za-z]))[0-9A-Za-z]+".r
    s"($Word(?:-$Word)*?)(?:-(debug|error))?(?:\\.log|-\\d{4}-\\d{2}-\\d{2}(?:-\\d+)?\\.log\\.gz)(?:${
      quote(TmpSuffix)})?".r

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

  def isOurTmpFile(file: Path): Boolean =
    file.toString.endsWith(LogGzTmpSuffix)

  // For external Java callers
  def isOurLogFilenameJava(isValidPrefix: String --> Boolean, file: Path): Boolean =
    isOurLogFilename(isValidPrefix.asScala, file)

  def isOurLogFilename(isValidPrefix: String => Boolean, file: Path): Boolean =
    fileToPrefixAndLogLevel(file) match
      case Some((prefix, _)) => isValidPrefix(prefix)
      case _ => false

  def fileToPrefixAndLogLevel(file: Path): Option[(String, LogLevel)] =
    file.getFileName.toString match
      case NameRegex(prefix: String, logLevel) =>
        logLevel match
          case null => Some(prefix -> LogLevel.Info)
          case logLevel: String =>
            Try(LogLevel(logLevel.capitalize)).toOption.map(prefix -> _)
      case _ => None
