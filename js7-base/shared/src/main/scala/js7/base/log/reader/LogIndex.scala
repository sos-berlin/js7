package js7.base.log.reader

import cats.effect.std.Supervisor
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Stream}
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.concurrent.ConcurrentSkipListMap
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.bytesToPosAndLines
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.watch.DirectoryEvent
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogIndex.*
import js7.base.log.reader.LogIndexBuilder.{LogFileDeleted, LogFileEvent, LogFileIndexDeleted, LogFilePending}
import js7.base.log.reader.LogLineKey
import js7.base.log.reader.recompressors.Recompressor
import js7.base.log.{Logger, reader}
import js7.base.problem.Problems.IncompleteLogFileProblem
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{ConcurrentHashMap, Delayer, ScalaUtils}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.Ordered.orderingToOrdered

/** Provides continuous streams of log lines from a log spreaded over multiple files.
  *
  * LogIndex handles growing log files and compressed (archived) log files.
  * A LogIndex spans several LogFileIndex, one for each file.
  *
  * Streaming may start at an Instant or a LogLineKey.
  *
  * See LogDirectoryIndex, which watches a directory and provides a LogIndex for
  * each pair of logFilePrefix and LogLevel.
  *
  * @param logFileEvents updates the file list, must emit events only from `directory`
  * @param initialPendingFiles log files whose timestamp (in the first log line) has not yet been read.
  *                            The file may still be written in the background.
  *                            The timestamp is read when a file is requested
  *                            (see `resolvePending`).
  * @param watchGrowth when growing log files should be respected (uncompressed only)
  */
final class LogIndex private(
  initialFiles: Iterable[LogFile],
  initialPendingFiles: Iterable[Path],
  logFileEvents: Stream[IO, LogFileEvent],
  breakLinesLongerThan: Option[Int],
  watchGrowth: Boolean,
  label: String,
  fileAddedSignal: SignallingRef[IO, EpochNano],
  pendingCount: SignallingRef[IO, Int]) // Number of pendingFiles, to start and stop polling
  (using
    zoneId: ZoneId, conf: LogIndexConf)
extends Service.StoppableByCancel:

  private given Recompressor = conf.recompressor

  private val instantToLogFile: ConcurrentSkipListMap[Instant, LogFile] =
    ConcurrentSkipListMap(initialFiles.toKeyedMap(_.fileInstant).asJava)
  private val fileToInstant: ConcurrentHashMap[Path, Instant] =
    ConcurrentHashMap.from:
      instantToLogFile.asScala.map: (instant, logFile) =>
        logFile.filename -> instant
      .toMap

  private val pendingFiles = ConcurrentHashMap.from:
    initialPendingFiles.map(f => f.filename -> f)

  protected def startService =
    runService:
      run.guarantee:
        release

  private def run =
    // Cancelled when service is stopping
    logFileEvents.chunks.evalMap: chunk =>
      val logLines = mutable.Buffer[String]()
      chunk.traverse:
        case event @ LogFilePending(file) =>
          logLines += event.toString
          IO:
            pendingFiles.put(file.filename, file)
          .flatMap: replaced =>
            IO.whenA(replaced.isEmpty):
              pendingCount.update(_ + 1)

        case event @ LogFileDeleted(filename) =>
          IO.uncancelable: _ =>
            removePending(filename).flatMap: _ =>
              removeLogFile(filename)
            .map: removed =>
              logLines += event.toString + removed.fold("")(o => s", remove $o")

        case event @ LogFileIndexDeleted(filename) =>
          // TODO Test is missing
          logLines += event.toString
          val originalFilename = Paths.get(filename.toString.stripSuffix(LogUtils.TmpSuffix))
          fileToInstant.get(originalFilename)
            .flatMap(instant => Option(instantToLogFile.get(instant)))
            .traverse: logFile =>
              IO.uncancelable: _ =>
                logFile.releaseIndex(deleteTmpFile = false)
      *> IO:
        logLines.foreachWithBracket()((line, br) => logger.info(s"$br$line"))
    .compile.drain

  /** Try once to read the timestamp of each pending log file.
    *
    * Files still lacking their first log line (because they are written in background)
    * stay pending. */
  private def resolvePending: IO[Unit] =
    IO(pendingFiles.values.toVector).flatMap: pending =>
      Stream.iterable(pending)
        .parEvalMapUnordered(conf.timestampReaderConcurrency): file =>
          LogFile.read(file).flatMap:
            case Left(problem: IncompleteLogFileProblem) =>
              IO(logger.debug(s"${file.getFileName} is still pending due to: $problem"))

            case Left(problem) =>
              removePending(file.filename) *>
                IO(logger.error:
                  s"${file.getFileName} is not readable or not a valid log file: $problem")

            case Right(logFile) =>
              removePending(file.filename).flatMap: wasPending =>
                // Not pending anymore if the file has been deleted in the meantime
                IO.whenA(wasPending):
                  addLogFile(logFile) *>
                    // The LogFileDeleted event may have been handled between removePending and
                    // addLogFile, in which case it did not find the file to remove.
                    removeLogFileIfDeleted(logFile)
        .compile.drain

  private def removeLogFileIfDeleted(logFile: LogFile): IO[Unit] =
    IO.blocking(Files.exists(logFile.originalFile)).flatMap: exists =>
      IO.unlessA(exists):
        IO.uncancelable: _ =>
          removeLogFile(logFile.filename).map: removed =>
            logger.info(s"${logFile.filename} has been deleted while being added${
              removed.fold("")(o => s", remove $o")}")

  /** @return the removed LogFile, if any. */
  private def removeLogFile(filename: Path): IO[Option[LogFile]] =
    IO.defer:
      fileToInstant.remove(filename)
        .filter: instant =>
          Option(instantToLogFile.get(instant)).exists(_.filename == filename)
        .flatMap: instant =>
          Option(instantToLogFile.remove(instant))
        .traverse: logFile =>
          logFile.releaseIndex.as(logFile)

  /** @return true iff the file was pending. */
  private def removePending(filename: Path): IO[Boolean] =
    IO:
      pendingFiles.remove(filename).isDefined
    .flatTap: wasPending =>
      IO.whenA(wasPending):
        pendingCount.update(_ - 1)

  private def addLogFile(logFile: LogFile): IO[Unit] =
    IO.uncancelable: _ =>
      IO.defer:
        var logLine = s"Added $logFile"
        val replaced = instantToLogFile.put(logFile.fileInstant, logFile)
        Option(replaced).foldMapM: replacedLogFile =>
          logLine += s", replace ${replacedLogFile.filename}"
          fileToInstant.remove(replacedLogFile.filename)
          replacedLogFile.releaseIndex
        *> IO.defer:
          fileToInstant.put(logFile.filename, logFile.fileInstant)
          logger.info(logLine)
          fileAddedSignal.set(logFile.fileEpochNano)

  private def release =
    IO.defer:
      instantToLogFile.values.asScala.toVector.parFoldMapA: logFile =>
        logFile.releaseIndex

  def byteLineStream(begin: Instant | LogLineKey, logSelection: LogSelection)
  : Stream[IO, Chunk[Byte]] =
    keyedByteLogLineStream(begin, logSelection).map:
      _.byteLine

  def stringLineStream(begin: Instant | LogLineKey, logSelection: LogSelection)
  : Stream[IO, String] =
    keyedByteLogLineStream(begin, logSelection).map:
      _.lineAsString

  /** Returns the LogLineKey corresponding to the given instant.
    *
    * @return None if no log file exists.
    *         Otherwise the LogLineKey of the instant of an instant that would be at this position.
    */
  def instantToLogLineKey(instant: Instant, logSelection: LogSelection): IO[Option[LogLineKey]] =
    keyedByteLogLineStream(instant, logSelection)
      .head.compile.last.map(_.map(_.logLineKey))

  def keyedByteLogLineStream(begin: Instant | LogLineKey, logSelection: LogSelection)
  : Stream[IO, KeyedByteLogLine] =
    val forReader = logSelection.forReader
    Stream.eval(resolvePending).flatMap: _ =>
      begin.match
        case instant: Instant =>
          instantToLogFile(instant).map: logFile =>
            logFile -> streamFileFromInstant(logFile, instant, forReader)

        case LogLineKey(fileInstant, position) =>
          instantToLogFile(fileInstant).map: logFile =>
            logFile -> streamFileFromPos(logFile, position, forReader)
    .flatMap: (logFile, stream) =>
      stream ++
        nextFilesToKeyedLines(logFile.fileInstant, forReader)
    .through:
      logSelection.pipe

  /** @return Empty Stream iff instantToLogFile is empty, otherwise a single element. */
  private def instantToLogFile(instant: Instant): Stream[fs2.Pure, LogFile] =
    Stream.suspend:
      Stream.fromOption:
        instantToLogFile.floorEntry(instant) match
          case null => Option(instantToLogFile.firstEntry).map(_.getValue)
          case o => Some(o.getValue)

  private def streamFileFromInstant(
    logFile: LogFile, begin: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      if !forReader.backwards && Option(instantToLogFile.firstKey).forall(begin < _) then
        // No recompression and indexing needed. This returns also the header line.
        wholeFile(logFile, forReader)
      else
        Stream.eval:
          toDeferredIndex(logFile)
        .flatMap: deferredIndex =>
          deferredIndex.logFileIndex.instantToLines(begin, forReader)
        .map: posAndLine =>
          KeyedByteLogLine(logFile.fileInstant, posAndLine)

  private def streamFileFromPos(
    logFile: LogFile, position: Long, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    locally:
      if !forReader.backwards && position == 0 then
        wholeFileForward(logFile, forReader)
      else
        Stream.eval:
          // We must recompress (but not necessarily index) to return positions of the recompressed file
          toDeferredIndex(logFile)
        .flatMap: deferredIndex =>
          if forReader.backwards || logFile.isGzipped then
            // LogLineIndex converts the (uncompressed) position into an OpaquePos in the compressed file
            deferredIndex.logFileIndex.positionToLines(position, forReader)
          else
            locally:
              if forReader.growing then
                LogFileReader.streamGrowingLogFile(
                  deferredIndex.file,
                  byteChunkSize = forReader.byteChunkSize,
                  poll = conf.pollGrowing,
                  position = position)
              else
                ByteSeqFileReader.streamFromPosition[Chunk[Byte]](
                  deferredIndex.file,
                  position = position,
                  byteChunkSize = forReader.byteChunkSize)
            .through:
              bytesToPosAndLines(
                fromPosition = position,
                breakLinesLongerThan = breakLinesLongerThan,
                PosAndLine(_, _))
    .map: posAndLine =>
      KeyedByteLogLine(logFile.fileInstant, posAndLine)

  private def nextFilesToKeyedLines(lastFileInstant: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    nextLogFile(lastFileInstant, forReader).flatMap: logFile =>
      wholeFile(logFile, forReader) ++
        nextFilesToKeyedLines(logFile.fileInstant, forReader)

  private def nextLogFile(lastFileInstant: Instant, forReader: LogSelection.ForReader) =
    Stream.eval:
      IO.whenA(watchGrowth && forReader.growing):
        val lastFileEpochNano = lastFileInstant.toEpochNano
        // Pending files are polled only while we are waiting for a new file
        fileAddedSignal.waitUntil(_ > lastFileEpochNano)
          .race(pollPending)
          .void
      *>
        IO:
          Option:
            if forReader.backwards then
              instantToLogFile.lowerEntry(lastFileInstant)
            else
              instantToLogFile.higherEntry(lastFileInstant)
          .map(_.getValue)
    .unNoneTerminate

  /** Poll only while there are pending files, otherwise wait idle for one. Never ends. */
  private def pollPending: IO[Unit] =
    logger.debugIO:
      pendingCount.discrete.map(_ > 0).changes
        .switchMap: hasPending =>
          if hasPending then
            Delayer.stream[IO](conf.logFileTimestampTries).evalMap: _ =>
              resolvePending
          else
            Stream.empty
        .compile.drain

  private def wholeFile(logFile: LogFile, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    locally:
      if forReader.backwards then
        wholeFileReverse(logFile, forReader)
      else
        wholeFileForward(logFile, forReader)
    .map: posAndLine =>
      KeyedByteLogLine(logFile.fileInstant, posAndLine)

  private def wholeFileForward(logFile: LogFile, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    locally:
      if logFile.isGzipped then
        Stream.force:
          logFile.maybeLogFileIndex.map:
            case Some(logFileIndex) if conf.recompressor.isFast =>
              logFileIndex.wholeFile(forReader)
            case _ =>
              logFile.toGzipDecompressingStream(forReader.byteChunkSize)
      else if forReader.growing then
        LogFileReader.streamGrowingLogFile(
          logFile.originalFile,
          byteChunkSize = forReader.byteChunkSize,
          poll = conf.pollGrowing)
      else
        ByteSeqFileReader.stream(logFile.originalFile, byteChunkSize = forReader.byteChunkSize)
    .prefetch
    .through:
      byteChunksToLines(breakLinesLongerThan = breakLinesLongerThan)
    .through:
      bytesToPosAndLines(fromPosition = 0, breakLinesLongerThan = breakLinesLongerThan,
        PosAndLine(_, _))

  private def wholeFileReverse(logFile: LogFile, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    Stream.eval:
      toDeferredIndex(logFile)
    .flatMap: deferredIndex =>
      deferredIndex.logFileIndex.positionToLines(position = Long.MaxValue, forReader)

  private def toDeferredIndex(logFile: LogFile): IO[LogFile.DeferredIndex] =
    logFile.toDeferredIndex(pollGrowing = watchGrowth ? conf.pollGrowing)

  /** Includes pending files, whose timestamp has not yet been read (listed last). */
  def files: Seq[Path] =
    instantToLogFile.values.asScala.toVector.sortBy(_.fileInstant).map(_.originalFile) ++
      pendingFiles.values.toVector.sorted

  override def toString =
    s"LogIndex($label, ${instantToLogFile.size} files)"


object LogIndex:
  private val logger = Logger[LogIndex]

  /** LogIndex for a single log spreaded over multiple files,
    * with initial files and a Stream of DirectoryEvents. */
  private[reader] def directory(
    directory: Path,
    files: Seq[Path],
    directoryEvents: Stream[IO, DirectoryEvent],
    watchGrowth: Boolean,
    label: String)
    (using zoneId: ZoneId, conf: LogIndexConf)
  : ResourceIO[LogIndex] =
    logger.traceResource(s"LogIndex $directory $label"):
      given Recompressor = conf.recompressor
      for
        (logFiles, pendingFiles, pipe) <-
          Resource.eval:
            LogIndexBuilder.toLogFilesAndEvents(directory, files)
        logFileIndex <- resource(
          logFiles,
          pendingFiles,
          directoryEvents.through(pipe),
          label = label,
          watchGrowth = watchGrowth)
      yield
        logger.whenDebugEnabled:
          logFiles.sorted.map(_.toStringWithSize).foreachWithBracket(): (line, br) =>
            logger.debug(s"$br$line")
        logFileIndex

  def files(files: Iterable[Path], watchGrowth: Boolean = false, label: String)
    (using zoneId: ZoneId, conf: LogIndexConf)
  : ResourceIO[LogIndex] =
    given Recompressor = conf.recompressor
    for
      logFiles <- Resource.eval:
        Stream.iterable(files).parEvalMap(sys.runtime.availableProcessors): file =>
          LogFile.read(file).orThrow
        .compile.toVector.map: logFiles =>
          logFiles.view.map(_.toStringWithSize).foreachWithBracket(): (line,br) =>
            logger.debug(s"$br$line")
          logFiles
      logIndex <- resource(logFiles, Nil, Stream.empty, watchGrowth = watchGrowth, label = label)
    yield
      logIndex

  private def resource(
    initialLogFiles: Iterable[LogFile],
    initialPendingFiles: Iterable[Path],
    logFileEvents: Stream[IO, LogFileEvent],
    label: String,
    watchGrowth: Boolean)
    (using ZoneId, LogIndexConf)
  : ResourceIO[LogIndex] =
    logger.traceResource("resource", label):
      for
        given Supervisor[IO] <- Supervisor[IO]
        logFileIndex <- Resource.suspend:
          for
            signal <- SignallingRef[IO, EpochNano]:
              initialLogFiles.map(_.fileEpochNano).maxOption getOrElse EpochNano.MinValue
            pendingCount <- SignallingRef[IO, Int]:
              initialPendingFiles.map(_.filename).toSet.size
          yield
            Service:
              LogIndex(
                initialLogFiles,
                initialPendingFiles,
                logFileEvents,
                breakLinesLongerThan = Some(summon[LogIndexConf].logFileIndexLineLength),
                watchGrowth = watchGrowth,
                label = label,
                signal,
                pendingCount)
      yield
        logFileIndex

  private[reader] def isGzipped(file: Path): Boolean =
    val name = file.getFileName.toString
    name.endsWith(".log.gz") || name.endsWith(LogUtils.LogGzTmpSuffix)


  sealed trait LogDirectoryIndexMXBean:
    this: Bean.type =>

    def getTmpFilesSize: Long =
      tmpFilesSize


  object Bean extends LogDirectoryIndexMXBean:
    protected[reader] var tmpFilesSize: Long = 0
