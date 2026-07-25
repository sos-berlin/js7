package js7.base.log.reader

import cats.effect.std.{AtomicCell, Queue, Semaphore, Supervisor}
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import fs2.Stream
import fs2.concurrent.SignallingRef
import java.nio.file.Path
import java.time.ZoneId
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.catsutils.{CatsDeadline, FiberVar}
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.DirectoryEvent
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.{LogFile, isGzipped}
import js7.base.log.reader.LogDirectoryIndexBuilder.*
import js7.base.log.reader.recompressors.LogFileIndexConf
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.Delayer
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.TimeoutException

private final class LogDirectoryIndexBuilder private(
  logFileTimestampSempahore: Semaphore[IO],
  supervisor: Supervisor[IO],
  gzLogFileReady: SignallingRef[IO, CatsDeadline])
  (using zoneId: ZoneId, conf: LogFileIndexConf):

  private def toLogFileEvents(directory: Path, initialFiles: Seq[Path])
  : IO[(Seq[LogFile], fs2.Pipe[IO, DirectoryEvent, LogFileEvent])] =
    for
      queue <- Queue.unsafeUnbounded[IO, LogFile]
      (logFiles, delayedLogFiles) <- toLogDelayedFiles(initialFiles, queue.offer)
      filenameToDelayedLogFile <- AtomicCell[IO].of:
        delayedLogFiles.toKeyedMap(_.filename)
    yield
      logFiles ->
        (stream => toLogFileEvents2(directory, stream, filenameToDelayedLogFile, queue))

  /** Try to extract the timestamp of the first line of each file. */
  private def toLogDelayedFiles(files: Iterable[Path], onCompleted: LogFile => IO[Unit])
  : IO[(Vector[LogFile], Vector[DelayedLogFile])] =
    Stream.iterable(files)
      .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
        LogFile.read(file).flatMap:
          case Left(problem) =>
            logger.debug(s"💥 Delaying ${file.getFileName} due to: $problem")
            val a = DelayedLogFile(file)
            // Read the timestamp of the first line of the log file in the background.
            a.start(onCompleted).as(Right(a))
          case Right(logFile) =>
            IO.left(logFile)
      .compile.toVector
      .map(_.partitionMap(identity))

  private def toLogFileEvents2(
    directory: Path,
    stream: Stream[IO, DirectoryEvent],
    filenameToDelayedLogFile: AtomicCell[IO, Map[Path, DelayedLogFile]],
    queue: Queue[IO, LogFile])
  : Stream[IO, LogFileEvent] =
    Stream.fromQueueUnterminated:
      queue
    .map:
      LogFileAdded(_)
    .mergeHaltR:
      stream.evalMap:
        case event @ FileAdded(filename) =>
          logger.debug(s"-->$event")
          val file = directory.resolve(filename)
          // Try to read the timestamp of .log-file forever (or until cancelled) in background.
          // When the timestamp has been read, enqueue a LogFileAdded event.
          val delayedLogFile = DelayedLogFile(file)
          filenameToDelayedLogFile.modify: map =>
            map.updated(filename, delayedLogFile) -> map.get(filename)
          .flatMap:
            case Some(forgotten: DelayedLogFile) =>
              forgotten.cancel/*Should not happen*/
            case _ => IO.unit
          .flatMap: _ =>
            // Read log file's timestamp in background
            delayedLogFile.start(queue.offer, delay = true)
          .as(Nil)

        case event @ FileDeleted(filename) =>
          logger.debug(s"-->$event")
          if filename.toString.endsWith(LogDirectoryIndex.TmpSuffix) then
            IO.pure(LogFileIndexDeleted(filename) :: Nil)
          else if isGzipped(filename) then
            IO.pure(LogFileDeleted(filename) :: Nil)
          else
            // If filename is .log-file which timestamp has not been read until it has
            // been deleted, then we cancel the fiber.
            filenameToDelayedLogFile.modify: map =>
              map.removed(filename) -> map.get(filename)
            .flatMap:
              case Some(delayedLogFile: DelayedLogFile) =>
                delayedLogFile.cancel.map:
                  // Return LogFileDeleted only after a LogFileAdded has been emitted
                  // (the timestamp has been read while the file has been deleted).
                  !_ thenList LogFileDeleted(filename)
              case _ =>
                IO.pure:
                  LogFileDeleted(filename) :: Nil

        case o: FileModified =>
          sys.error(s"LogDirectoryIndex: unexpected $o")
      .flatMap:
        Stream.iterable

  /** Try to read a log file's timestamp with logFileTimestampTries duration
    *
    * The second line (the first line log line after the header line) contains the timestamp.
    *
    * Because log4j may still writing this file, it may take some milliseconds.
    */
  private def toLogFileUntilFinished(file: Path): IO[Option[LogFile]] =
    logger.debugIOWithResult("toLogFileUntilFinished", file.getFileName):
      Delayer.stream[IO](conf.logFileTimestampTries, finite = true)
        .evalMap: _ =>
          logFileTimestampSempahore.permit.surround:
            LogFile.read(file)
        .flatMap:
          case Left(problem) =>
            logger.debug(s"toLogFileUntilFinished ⟲ $file: $problem")
            Stream.empty
          case Right(logFile) =>
            //logger.trace(s"${logFile.toStringWithSize}")
            Stream.emit(logFile)
        .head
        .compile.last


  private[reader] final class DelayedLogFile(val file: Path):
    val filename: Path = file.filename
    private val completed = Deferred.unsafe[IO, Unit]
    private val fiberVar = FiberVar[Unit]

    /** Try to read the timestamp of a log-file forever (or until cancelled) in background. */
    def start(
      onCompleted: LogFile => IO[Unit],
      onFailed: Throwable => IO[Unit] = _ => IO.unit,
      delay: Boolean = false)
    : IO[Unit] =
      logger.trace(s"DelayedLogFile($filename).start")
      val gzip = isGzipped(file)
      // Read log file's timestamp in background
      supervisor.supervise:
        IO.whenA(delay):
          IO.whenA(!gzip):
            CatsDeadline.now.flatMap: now =>
              gzLogFileReady.waitUntil(_ >= now).timeoutTo(conf.currentFileMaxDelay, IO.unit)
          *>
            IO.sleep(conf.fileAddedDelay)
        .productR:
          toLogFileUntilFinished(file)
            .logWhenItTakesLonger(filename.toString)
            .flatMap:
              case None =>
                logger.error(s"$filename is not readable, giving up")
                onFailed(new TimeoutException(s"LogFile $filename read timed out after $delay"))
              case Some(logFile) =>
                completed.complete(()).flatMap:
                  IO.whenA(_): // not cancelled
                    IO.whenA(gzip):
                      CatsDeadline.now.flatMap:
                        gzLogFileReady.set
                    *> onCompleted(logFile)
            .handleErrorWith: t =>
              logger.error(s"$filename: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
              onFailed(t)
      .flatMap:
        fiberVar.set

    /** Cancel and return true iff this has completed the background operation. */
    def cancel: IO[Boolean] =
      fiberVar.cancel *>
        completed.complete(())


    override def toString = s"DelayedLogFile($filename)"


private object LogDirectoryIndexBuilder:

  private val logger = Logger[this.type]

  /** Return the initial LogFiles and a FS2 Pipe converting a stream of DirectoryEvent
    * to a stream of LogFile (containing their timestamps). */
  def toLogFileEvents(directory: Path, initialFiles: Seq[Path])
    (using zoneId: ZoneId, conf: LogFileIndexConf)
  : ResourceIO[(Seq[LogFile], fs2.Pipe[IO, DirectoryEvent, LogFileEvent])] =
    for
      supervisor <- Supervisor[IO]
      result <- Resource.eval:
        for
          semaphore <- Semaphore[IO](conf.timestampReaderConcurrency)
          now <- CatsDeadline.now
          signal <- SignallingRef[IO, CatsDeadline](now - conf.currentFileMaxDelay)
          result <- LogDirectoryIndexBuilder(semaphore, supervisor, signal)
            .toLogFileEvents(directory, initialFiles)
        yield
          result
    yield
      result

  @TestOnly
  private[reader] def forTest(directory: Path)(using zoneId: ZoneId, conf: LogFileIndexConf)
  : ResourceIO[LogDirectoryIndexBuilder] =
    for
      supervisor <- Supervisor[IO]
      result <- Resource.eval:
        for
          semaphore <- Semaphore[IO](conf.timestampReaderConcurrency)
          now <- CatsDeadline.now
          signal <- SignallingRef[IO, CatsDeadline](now - conf.currentFileMaxDelay)
        yield
          LogDirectoryIndexBuilder(semaphore, supervisor, signal)
    yield
      result

  private[reader] sealed trait LogFileEvent

  private[reader] final case class LogFileAdded(logFile: LogFile) extends LogFileEvent

  private[reader] final case class LogFileDeleted(filename: Path) extends LogFileEvent

  private[reader] final case class LogFileIndexDeleted(filename: Path) extends LogFileEvent
