package js7.base.log.reader

import cats.effect.std.{AtomicCell, Queue, QueueSink, Semaphore, Supervisor}
import cats.effect.{Deferred, FiberIO, IO, Resource, ResourceIO}
import cats.syntax.option.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import java.io.{EOFException, FileInputStream, FileNotFoundException}
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.zip.GZIPInputStream
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.catsutils.{CatsDeadline, FiberVar}
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.io.file.watch.DirectoryEvent
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogDirectoryIndexBuilder.*
import js7.base.log.reader.recompressors.LogFileIndexConf
import js7.base.problem.{Checked, Problem}
import js7.base.time.EpochNano
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Delayer}

private final class LogDirectoryIndexBuilder private(
  logFileTimestampSempahore: Semaphore[IO],
  supervisor: Supervisor[IO],
  gzLogFileReady: SignallingRef[IO, CatsDeadline])
  (using zoneId: ZoneId, conf: LogFileIndexConf):

  private def toLogFileEvents(directory: Path, initialFiles: Seq[Path])
  : IO[(Seq[LogFile], fs2.Pipe[IO, DirectoryEvent, LogFileEvent])] =
    for
      queue <- Queue.unsafeUnbounded[IO, LogFile]
      (logFiles, delayedLogFiles) <- toLogDelayedFiles(initialFiles, queue)
      filenameToDelayedLogFile <- AtomicCell[IO].of:
        delayedLogFiles.toKeyedMap(_.filename)
    yield
      logFiles ->
        (stream => toLogFileEvents2(directory, stream, filenameToDelayedLogFile, queue))

  /** Try to extract the timestamp of the first line of each file. */
  private def toLogDelayedFiles(files: Iterable[Path], queue: QueueSink[IO, LogFile])
  : IO[(Vector[LogFile], Vector[DelayedLogFile])] =
    Stream.iterable(files)
      .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
        toLogFile(file).flatMap:
          case Left(problem) =>
            logger.debug(s"Delaying ${file.getFileName} due to: $problem")
            val a = DelayedLogFile(file, onCompleted = queue.offer)
            // Read the timestamp of the first line of the plain log file in the background.
            a.start().as(Right(a))
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
          val delayedLogFile = DelayedLogFile(file, onCompleted = queue.offer)
          filenameToDelayedLogFile.modify: map =>
            map.updated(filename, delayedLogFile) -> map.get(filename)
          .flatMap:
            case Some(forgotten: DelayedLogFile) =>
              forgotten.cancel/*Should not happen*/
            case _ => IO.unit
          .flatMap: _ =>
            // Read log file's timestamp in background
            delayedLogFile.start(delay = true)
          .as(Nil)

        case event @ FileDeleted(filename) =>
          logger.debug(s"-->$event")
          if isGzipped(filename) then
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
  end toLogFileEvents2

  /** Try to read a log file's timestamp with logFileTimestampTries duration
    *
    * The second line (the first line log line after the head line) contains the timestamp.
    *
    * Because log4j may still writing this file, it may take some milliseconds.
    */
  private def toLogFileUntilFinished(file: Path): IO[Option[LogFile]] =
    logger.debugIOWithResult("toLogFileUntilFinished", file.getFileName):
      Delayer.stream[IO](conf.logFileTimestampTries, finite = true)
        .evalMap: _ =>
          logFileTimestampSempahore.permit.surround:
            toLogFile(file)
        .flatMap:
          case Left(problem) =>
            logger.debug(s"toLogFileUntilFinished ⟲ $file: $problem")
            Stream.empty
          case Right(logFile) =>
            //logger.trace(s"${logFile.toStringWithSize}")
            Stream.emit(logFile)
        .head
        .compile.last


  private final class DelayedLogFile(val file: Path, onCompleted: LogFile => IO[Unit]):
    val filename: Path = file.getFileName
    private val completed = Deferred.unsafe[IO, Unit]
    private val fiberVar = FiberVar[Unit]

    /** Try to read the timestamp of a log-file forever (or until cancelled) in background. */
    def start(delay: Boolean = false): IO[Unit] =
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
                IO(logger.error(s"$filename is not readable, giving up"))
              case Some(logFile) =>
                completed.complete(()).flatMap:
                  IO.whenA(_):
                    IO.whenA(gzip):
                      CatsDeadline.now.flatMap:
                        gzLogFileReady.set
                    *> onCompleted(logFile)
            .handleError: t =>
              logger.error(s"$filename: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
      .flatMap:
        fiberVar.set

    /** Cancel and return true iff this has completed the background operation. */
    def cancel: IO[Boolean] =
      fiberVar.cancel *>
        completed.complete(())


    override def toString = s"DelayedLogFile($filename)"


private object LogDirectoryIndexBuilder:

  private val logger = Logger[this.type]
  /** First chunk of log file must include the timestamp of the second line
    * (the line after the header) */
  private val FirstChunkSize = 1024

  /** Convert a stream of DirectoryEvent to a stream of LogFile (containing their timestamps).
    *
    * log4j is expected to archive log files in this order (assuming log4j is already running):
    * * Write footer line and close .log-file
    * * LogFileAdded(.log.gz) —  _.log is completed, be prepared for a new .log.gz_
    * * Write .log.gz
    * * LogFileDeleted(.log) —  _.log.gz is completely written, it is readable now_
    * * LogFileAdded(.log), same filename —  _expect the first log line with a timestamp_
    * * Write first log line (containing the first timestamp)
    */
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

  /** Extract the timestamp of the first line of a log file and return a [[LogFile]].
    */
  private[reader] def toLogFile(file: Path)(using ZoneId): IO[Checked[LogFile]] =
    val gzip = isGzipped(file)
    readLogFileInstant(file, gzip).flatMapT: instant =>
      AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).map: cell =>
        Right:
          LogFile(file, instant, cell, isGzipped = gzip)

  private def readLogFileInstant(file: Path, gzip: Boolean)(using ZoneId): IO[Checked[Instant]] =
    Resource.fromAutoCloseable:
      IO.blocking:
        if gzip then
          GZIPInputStream(FileInputStream(file.toFile), 1024)
        else
          FileInputStream(file.toFile)
    .use: in =>
      IO.blocking:
        ByteArray.unsafeWrap:
          in.readNBytes(FirstChunkSize)
    .map: chunk =>
      def eofProblem = Problem.pure("LogFileRegister readLogFileInstant: EOF")
      chunk.indexOf('\n') match
        case lf if lf > 15 /*minimum length of headline*/ =>
          val lineEnd = chunk.indexOf('\n', lf + 1)
          if lineEnd == -1 then
            Left(eofProblem)
          else
            // Timestamp of first log line after the header line
            FastTimestampParser().parseTimestampInLogLine(chunk.slice(lf + 1, lineEnd))
              .toOption.toRight(Problem("Invalid timestamp"))
              .map(_.toInstant)
        case _ =>
          Left(eofProblem)
    .recover: throwable =>
      if throwable.getStackTrace != null then
        throwable match
          case _: (FileNotFoundException | EOFException) =>
          case _ =>
            logger.debug(s"❓readLogFileInstant $file: ${throwable.toStringWithCauses}", throwable)
      Left(Problem.fromThrowable(throwable))


  private[reader] sealed trait LogFileEvent

  private[reader] final case class LogFileAdded(logFile: LogFile) extends LogFileEvent:
    def filename = logFile.filename

  private[reader] final case class LogFileDeleted(filename: Path) extends LogFileEvent
