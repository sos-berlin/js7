package js7.base.log.reader

import cats.effect.IO
import fs2.Stream
import java.nio.file.Path
import java.time.ZoneId
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.DirectoryEvent
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.log.Logger
import js7.base.problem.Problems.IncompleteLogFileProblem

private object LogIndexBuilder:
  private val logger = Logger[this.type]

  /** Return the initial LogFiles, the initial files whose timestamp is not yet readable,
    * and a FS2 Pipe converting a stream of DirectoryEvent to a stream of LogFileEvent.
    *
    * The timestamp of the first log line of a pending file is not read here. The file may still
    * be written in the background. LogIndex reads it when it is requested to access its files. */
  def toLogFilesAndEvents(directory: Path, initialFiles: Seq[Path])
                         (using zoneId: ZoneId, conf: LogIndexConf)
  : IO[(
    initialLogFiles: Vector[LogFile],
    pendingFiles: Vector[Path],
    toLogFileEvents: fs2.Pipe[IO, DirectoryEvent, LogFileEvent]
  )] =
    toLogFiles(initialFiles).map: (logFiles, pendingFiles) =>
      (logFiles, pendingFiles, toLogFileEvents(directory))

  /** Try to extract the timestamp of the first line of each file, once. */
  private def toLogFiles(files: Iterable[Path])(using ZoneId, LogIndexConf)
  : IO[(Vector[LogFile], Vector[Path])] =
    Stream.iterable(files)
      .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
        LogFile.read(file).map:
          case Left(problem: IncompleteLogFileProblem) =>
            logger.debug(s"${file.getFileName} is pending due to: $problem")
            Some(Right(file))

          case Left(problem) =>
            logger.error(s"${file.getFileName} is not readable or not a valid log file: $problem")
            None

          case Right(logFile) =>
            Some(Left(logFile))
      .unNone
      .compile.toVector
      .map(_.partitionMap(identity))

  private def toLogFileEvents(directory: Path): fs2.Pipe[IO, DirectoryEvent, LogFileEvent] =
    _.map:
      case event @ FileAdded(filename) =>
        logger.debug(s"-->$event")
        if filename.toString.endsWith(LogUtils.TmpSuffix) then
          None // We added this file, nothing to do
        else
          // The timestamp will be read when LogIndex needs it
          Some(LogFilePending(directory.resolve(filename)))

      case event @ FileDeleted(filename) =>
        logger.debug(s"-->$event")
        if filename.toString.endsWith(LogUtils.TmpSuffix) then
          Some(LogFileIndexDeleted(filename))
        else
          Some(LogFileDeleted(filename))

      case o: FileModified =>
        sys.error(s"LogIndex: unexpected $o")
    .unNone

  private[reader] sealed trait LogFileEvent

  /** A new log file whose timestamp has not yet been read. */
  private[reader] final case class LogFilePending(file: Path) extends LogFileEvent:
    def filename: Path = file.filename

  private[reader] final case class LogFileDeleted(filename: Path) extends LogFileEvent

  private[reader] final case class LogFileIndexDeleted(filename: Path) extends LogFileEvent
