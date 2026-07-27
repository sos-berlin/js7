package js7.base.log.reader

import cats.effect.std.{AtomicCell, Supervisor}
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Stream}
import java.io.{EOFException, FileInputStream, FileNotFoundException}
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.concurrent.ConcurrentSkipListMap
import java.util.zip.GZIPInputStream
import js7.base.catsutils.CatsEffectExtensions.{orThrow, run}
import js7.base.catsutils.Environment.environment
import js7.base.config.Js7Conf
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.{inputStreamToStream, toPosAndLines}
import js7.base.io.OpaquePos
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryEvent, DirectoryState, DirectoryWatch}
import js7.base.io.file.{ByteSeqFileReader, FileDeleter}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogDirectoryIndexBuilder.{LogFileAdded, LogFileDeleted, LogFileEvent, LogFileIndexDeleted}
import js7.base.log.reader.recompressors.{LogFileIndexConf, Recompressor}
import js7.base.log.reader.{LogFileIndex, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.Problems.{IncompleteLogFileProblem, InvalidTimestampInLogFileProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ConcurrentHashMap, ScalaUtils}
import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.jdk.CollectionConverters.*
import scala.math.Ordered.orderingToOrdered
import scala.util.Try

/** Provides a continuous stream of log lines over all log files in the directory.
  * @param logFileEvents updates the file list, must emit events only from `directory`
  * @param watchGrowth when growing log files should be respected (uncompressed only)
  */
final class LogDirectoryIndex private(
  initialFiles: Iterable[LogFile],
  logFileEvents: Stream[IO, LogFileEvent],
  logLevel: LogLevel,
  recompressor: Recompressor,
  breakLinesLongerThan: Option[Int],
  watchGrowth: Boolean,
  fileAddedSignal: SignallingRef[IO, EpochNano])
  (using zoneId: ZoneId, conf: LogFileIndexConf)
extends Service.StoppableByCancel:

  private val instantToLogFile =
    ConcurrentSkipListMap(initialFiles.toKeyedMap(_.fileInstant).asJava)
  private val fileToInstant: ConcurrentHashMap[Path, Instant] =
    ConcurrentHashMap.from:
      instantToLogFile.asScala.toMap.map((instant, logFile) => logFile.filename -> instant)

  protected def start =
    startService:
      run.guarantee:
        release

  private def run =
    // Cancelled when service is stopping
    logFileEvents.chunks.evalMap: chunk =>
      val logLines = mutable.Buffer[String]()
      chunk.traverse:
        case event @ LogFileAdded(logFile) =>
          IO.uncancelable: _ =>
            IO.defer:
              fileToInstant.put(logFile.filename, logFile.fileInstant)
              val replaced = instantToLogFile.put(logFile.fileInstant, logFile)
              var logLine = event.toString
              Option(replaced).foldMap: replacedLogFile =>
                IO.whenA(replacedLogFile.filename != logFile.filename):
                  logLine = s"$event, replace ${replacedLogFile.filename}"
                  fileToInstant.remove(replacedLogFile.filename)
                  replacedLogFile.releaseIndex
              *> IO.defer:
                logLines += logLine
                fileAddedSignal.set(logFile.fileEpochNano)

        case event @ LogFileDeleted(filename) =>
          IO.uncancelable: _ =>
            var logLine = event.toString
            fileToInstant.remove(filename).foldMap: instant =>
              IO.whenA(Option(instantToLogFile.get(instant)).exists(_.filename == filename)):
                Option(instantToLogFile.remove(instant)).foldMap: logFile =>
                  logLine = s"$event, remove $logFile"
                  logFile.releaseIndex
            .map: _ =>
              logLines += event.toString

        case event @ LogFileIndexDeleted(filename) =>
          // TODO Test is missing
          logLines += event.toString
          val originalFilename = Paths.get(filename.toString.stripSuffix(TmpSuffix))
          fileToInstant.get(originalFilename)
            .flatMap(instant => Option(instantToLogFile.get(instant)))
            .traverse: logFile =>
              IO.uncancelable: _ =>
                logFile.releaseIndex(deleteFile = false)
      *> IO:
        logLines.foreachWithBracket()((line, br) => logger.info(s"$br$line"))
    .compile.drain

  private def release =
    IO.defer:
      instantToLogFile.values.asScala.toVector.parFoldMapA: logFile =>
        logFile.releaseIndex

  def byteLineStream(begin: Instant | LogLineKey, logSelection: LogSelection)
  : Stream[IO, Chunk[Byte]] =
    keyedByteLogLineStream(begin, logSelection).map:
      _.byteLine

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
    begin.match
      case instant: Instant =>
        Stream.suspend:
          instantToLogFile(instant).fold(Stream.empty): logFile =>
            Stream.emit:
              logFile -> fileToKeyedByteLogLines(logFile, instant, forReader)

      case LogLineKey(logLevel, fileInstant, position) =>
        Stream.suspend:
          if logLevel != this.logLevel then
            Stream.raiseError[IO](IllegalArgumentException("Wrong LogLevel"))
          else
            instantToLogFile(fileInstant).fold(Stream.empty): logFile =>
              Stream.emit:
                logFile -> fileToKeyedByteLogLines(logFile, position, forReader)
    .flatMap: (logFile, stream) =>
      stream ++
        nextFilesToKeyedLines(logFile.fileInstant, forReader)
    .through:
      logSelection.pipe

  /** @return None if instantToLogFile is empty. */
  private def instantToLogFile(instant: Instant): Option[LogFile] =
    instantToLogFile.floorEntry(instant) match
      case null => Option(instantToLogFile.firstEntry).map(_.getValue)
      case o => Some(o.getValue)

  private def fileToKeyedByteLogLines(
    logFile: LogFile, begin: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      if Option(instantToLogFile.firstKey).forall(begin < _) then
        // No recompression and indexing needed. This includes the header line.
        wholeFile(logFile, forReader)
      else
        Stream.eval:
          toDeferredIndex(logFile)
        .map:
          _.logFileIndex
        .flatMap: logFileIndex =>
          logFileIndex.instantToLines(begin, forReader)
        .map: posAndLine =>
          toKeyedByteLogLine(logFile.fileInstant, posAndLine)

  private def fileToKeyedByteLogLines(
    logFile: LogFile, position: Long, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      // We must recompress (and not necessarily index) to return positions of the recompressed file
      toDeferredIndex(logFile)
    .flatMap: deferredIndex =>
      if logFile.isGzipped then
        // LogLineIndex converts the (uncompressed) position into an OpaquePos in the compressed file
        deferredIndex.logFileIndex.positionToLines(position, forReader).map:
          toKeyedByteLogLine(logFile.fileInstant, _)
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
          toPosAndLines(firstPosition = position, breakLinesLongerThan = breakLinesLongerThan)
        .map: posAndLine =>
          toKeyedByteLogLine(logFile.fileInstant, PosAndLine.fromPair(posAndLine))

  private def nextFilesToKeyedLines(lastFileInstant: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    nextLogFile(lastFileInstant, forReader).flatMap: logFile =>
      wholeFile(logFile, forReader) ++
        nextFilesToKeyedLines(logFile.fileInstant, forReader)

  private def nextLogFile(lastFileInstant: Instant, forReader: LogSelection.ForReader) =
    Stream.eval:
      IO.whenA(watchGrowth && forReader.growing):
        val lastFileEpochNano = lastFileInstant.toEpochNano
        fileAddedSignal.waitUntil(_ > lastFileEpochNano)
      *>
        IO:
          Option:
            instantToLogFile.higherEntry(lastFileInstant)
          .map(_.getValue)
    .unNoneTerminate

  private def wholeFile(logFile: LogFile, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    locally:
      if logFile.isGzipped then
        // TODO Handle incomplete gzip file because it is still being written?
        toGzipDecompressingStream(logFile.originalFile, forReader.byteChunkSize)
      else if forReader.growing then
        LogFileReader.streamGrowingLogFile(
          logFile.originalFile,
          byteChunkSize = forReader.byteChunkSize,
          poll = conf.pollGrowing)
      else
        ByteSeqFileReader.stream(logFile.originalFile, byteChunkSize = forReader.byteChunkSize)
    .through:
      toPosAndLines(firstPosition = 0, breakLinesLongerThan = breakLinesLongerThan)
    .map: posAndLine =>
      toKeyedByteLogLine(logFile.fileInstant, PosAndLine.fromPair(posAndLine))

  private def toDeferredIndex(logFile: LogFile): IO[DeferredIndex] =
    logFile.deferredIndexCell.evalUpdateAndGet: maybe =>
      maybe.match
        case None =>
          buildIndex(logFile).toAllocated
        case Some(allo) =>
          if logFile.isGzipped &&
            !Files.exists(allo.allocatedThing.file)
          then
            logger.debug(s"${allo.allocatedThing.file.getFileName} deleted — rebuilding index")
            allo.release *> buildIndex(logFile).toAllocated
          else if logFile.isGzipped &&
            allo.allocatedThing.fileSize.exists(_.original != Files.size(logFile.originalFile))
          then
            // Happens when a compressed file has been indexed while compression hasn't finished
            logger.debug:
              s"${logFile.originalFile.getFileName} changed its size — rebuilding index"
            allo.release *> buildIndex(logFile).toAllocated
          else
            IO.pure(allo)
      .map(Some(_))
    .map(_.get.allocatedThing)

  private def buildIndex(logFile: LogFile): ResourceIO[DeferredIndex] =
    if logFile.isGzipped then
      buildIndexFromCompressedFile(logFile.originalFile)
    else
      buildIndexFromUncompressedFile(logFile.originalFile)

  /** Recompresses and indexes the gzFile. */
  private def buildIndexFromCompressedFile(gzFile: Path): ResourceIO[DeferredIndex] =
    Resource.suspend:
      IO:
        val t = Deadline.now
        val size = Files.size(gzFile)
        /** tmpFile contains the indexed, decompressed and maybe recompressed log file. */
        val tmpFile = Paths.get(gzFile.toString + TmpSuffix)
        Resource
          .make(
            acquire =
              LogFileIndex.fromStream(
                label = tmpFile.getFileName.toString,
                toBuilderStream = toGzipDecompressingStream(gzFile, _),
                toPositionedStream = (pos, forReader) =>
                  positionedTmpFileStream(tmpFile, pos, forReader.byteChunkSize, recompressor),
                logWriter = recompressor.toLogWriter(tmpFile)
              ).map: logFileIndex =>
                logger.info(s"Recompressed and indexed ${gzFile.getFileName}: ${
                  bytesPerSecondString(t.elapsed, logFileIndex.byteCount)}")
                logFileIndex)(
            release = _ =>
              IO.blocking:
                FileDeleter.tryDeleteFile(tmpFile))
        .map: logFileIndex =>
          val recompressedSize = Files.size(tmpFile)
          Bean.tmpFilesSize += recompressedSize
          DeferredIndex(logFileIndex, tmpFile, Some(size -> recompressedSize))

  private def buildIndexFromUncompressedFile(file: Path): ResourceIO[DeferredIndex] =
    if watchGrowth then
      LogFileIndex.buildGrowing(file, poll = conf.pollGrowing).map: logFileIndex =>
        DeferredIndex(logFileIndex, file)
    else
      Resource.eval:
        LogFileIndex.fromFile(file).map: logFileIndex =>
          DeferredIndex(logFileIndex, file)

  private def toKeyedByteLogLine(fileInstant: Instant, posAndLine: PosAndLine): KeyedByteLogLine =
    KeyedByteLogLine(logLevel, fileInstant, posAndLine)

  def files: Seq[Path] =
    instantToLogFile.values.asScala.toVector.map(_.originalFile)

  override def toString =
    s"LogDirectoryIndex($logLevel, ${instantToLogFile.size} files)"


object LogDirectoryIndex:
  private val logger = Logger[LogDirectoryIndex]
  private[reader] val LogGzTmpSuffix = ".log.gz" + TmpSuffix
  private[reader] val TmpSuffix = "-indexed.tmp"
  val LogLevels = Set(LogLevel.Error, LogLevel.Info, LogLevel.Debug)
  /** First chunk of log file must include the timestamp of the second line
    * (the line after the header) */
  private val FirstChunkSize = 1024

  /** LogDirectoryIndex, watching a directory. */
  def directory(
    directory: Path,
    filenamePrefix: String,
    logLevel: LogLevel,
    watchGrowth: Boolean)
    (using ZoneId, LogFileIndexConf)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(LogLevels(logLevel))
    Resource.suspend:
      watchDirectory(
        directory,
        isRelevantFile = LogUtils.isOurFilename(filenamePrefix, logLevel)
      ).map: (files, directoryEvents) =>
        this.directory(directory, logLevel, files, directoryEvents, watchGrowth = watchGrowth)

  /** LogDirectoryIndex, watching a directory. */
  private[reader] def directory(
    directory: Path,
    logLevel: LogLevel,
    files: Seq[Path],
    directoryEvents: Stream[IO, DirectoryEvent],
    watchGrowth: Boolean)
    (using zoneId: ZoneId, conf: LogFileIndexConf)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(LogLevels(logLevel))
    logger.traceResource("directory", (directory, logLevel)):
      for
        _ <- Resource.eval(deleteTmpFiles(directory, logLevel))
        (logFiles, pipe) <- LogDirectoryIndexBuilder.toLogFileEvents(directory, files)
        logFileIndex <- resource(
          logFiles,
          directoryEvents.through(pipe),
          logLevel,
          watchGrowth = watchGrowth,
          conf.recompressor)
      yield
        logger.whenTraceEnabled:
          logFiles.sorted.map(_.toStringWithSize).foreachWithBracket(): (line, br) =>
            logger.trace(s"$br$line")
        logFileIndex

  private[reader] def watchDirectory(directory: Path, isRelevantFile: Path => Boolean)
    (using conf: LogFileIndexConf)
  : IO[(Vector[Path], Stream[IO, DirectoryEvent])] =
    directory.directoryStream[IO]
      .filter:
        isRelevantFile
      .compile.toVector.map: files =>
        files ->
          DirectoryWatch.stream(
            directory,
            DirectoryState(files.map(_.filename)),
            conf.directoryWatchSettings.copy(watchDelay = 0.s),
            isRelevantFile,
            Set(ENTRY_CREATE, ENTRY_DELETE))

  def files(files: Iterable[Path], logLevel: LogLevel, watchGrowth: Boolean = false)
    (using zoneId: ZoneId, conf: LogFileIndexConf)
  : ResourceIO[LogDirectoryIndex] =
    for
      logFiles <- Resource.eval:
        Stream.iterable(files).parEvalMap(sys.runtime.availableProcessors): file =>
          LogFile.read(file).orThrow
        .compile.toVector.map: logFiles =>
          logFiles.view.map(_.toStringWithSize).foreachWithBracket(): (line,br) =>
            logger.trace(s"$br$line")
          logFiles
      logDirectoryIndex <- resource(logFiles, Stream.empty, logLevel, watchGrowth = watchGrowth,
        conf.recompressor)
    yield
      logDirectoryIndex

  private def resource(
    initialLogFiles: Iterable[LogFile],
    logFileEvents: Stream[IO, LogFileEvent],
    logLevel: LogLevel,
    watchGrowth: Boolean,
    recompressor: Recompressor)
    (using ZoneId, LogFileIndexConf)
  : ResourceIO[LogDirectoryIndex] =
    for
      given Supervisor[IO] <- Supervisor[IO]
      logFileIndex <- Resource.suspend:
        for
          js7Conf <- environment[Js7Conf]
          signal <- SignallingRef[IO, EpochNano]:
            initialLogFiles.map(_.fileEpochNano).maxOption getOrElse EpochNano.MinValue
        yield
          Service:
            LogDirectoryIndex(
              initialLogFiles,
              logFileEvents,
              logLevel, recompressor,
              breakLinesLongerThan = Some(js7Conf.logFileIndexLineLength),
              watchGrowth = watchGrowth,
              signal)
    yield
      logFileIndex

  private def toGzipDecompressingStream(gzFile: Path, bufferSize: Int): Stream[IO, Chunk[Byte]] =
    Stream.resource:
      Resource.fromAutoCloseable:
        IO.blocking:
          GZIPInputStream(
            FileInputStream(gzFile.toFile),
            bufferSize / 4 /*compression ratio*/)
    .flatMap: in =>
      inputStreamToStream(in, bufferSize = bufferSize)

  private def positionedTmpFileStream(
    file: Path,
    opaquePos: OpaquePos,
    bufferSize: Int,
    recompressor: Recompressor)
  : Stream[IO, Chunk[Byte]] =
    Stream.resource:
      Resource.fromAutoCloseable:
        IO.blocking:
          FileInputStream(file.toFile)
    .evalMap: (in: FileInputStream) =>
      IO.blocking:
        in.skip(opaquePos.toLong)
        recompressor.decompressingInputStream(in)
    .flatMap: in =>
      inputStreamToStream(in, bufferSize)

  private def deleteTmpFiles(directory: Path, logLevel: LogLevel): IO[Unit] =
    IO.blocking:
      FileDeleter.tryDeleteFiles:
        directory.directoryStream[SyncIO]
          .filter: file =>
            isOurTmpFile(file) && fileToLogLevel(file) == logLevel
          .compile.toVector
          .run()

  private[reader] def fileToLogLevel(file: Path): LogLevel =
    val name = file.getFileName.toString
    if name.contains("-error") then
      LogLevel.Error
    else if name.contains("-debug") then
      LogLevel.Debug
    else
      LogLevel.Info

  private[reader] def isGzipped(file: Path): Boolean =
    val name = file.getFileName.toString
    name.endsWith(".log.gz") || name.endsWith(LogGzTmpSuffix)

  private[reader] def isOurTmpFile(file: Path): Boolean =
    file.toString.endsWith(LogGzTmpSuffix)


  /** Description of a log file with its timestamp and a deferred `LogFileIndex`. */
  private[reader] final class LogFile private(
    val originalFile: Path,
    val fileInstant: Instant,
    val isGzipped: Boolean,
    private[LogDirectoryIndex] val deferredIndexCell:
      AtomicCell[IO, Option[Allocated[IO, DeferredIndex]]])
    (using zoneId: ZoneId):

    val filename: Path =
      originalFile.filename

    val fileEpochNano: EpochNano =
      fileInstant.toEpochNano

    def releaseIndex: IO[Unit] =
      releaseIndex(deleteFile = true)

    def releaseIndex(deleteFile: Boolean): IO[Unit] =
      deferredIndexCell.getAndSet(None).flatMap:
        _.foldMap: allo =>
          allo.allocatedThing.fileSize.foreach: o =>
            Bean.tmpFilesSize -= o.decompressed
          if deleteFile && allo.allocatedThing.file != originalFile then
            FileDeleter.tryDeleteFile(allo.allocatedThing.file)
          allo.release

    def toStringWithSize: String =
      s"$toString ${Try(toKBGB(Files.size(originalFile))).fold(identity, identity)}"

    override def toString =
      f"LogFile(${fileInstant.atZone(zoneId).toOffsetDateTime} -> $filename)"

  private[reader] object LogFile:
    given Ordering[LogFile] = Ordering.by(_.fileInstant)

    /** Extract the timestamp of the first line of a log file and return a [[LogFile]].
      */
    def read(file: Path)(using ZoneId): IO[Checked[LogFile]] =
      val gzip = isGzipped(file)
      readLogFileInstant(file, gzip).flatMapT: instant =>
        AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).map: cell =>
          Right:
            new LogFile(file, instant, isGzipped = gzip, cell)

    private def readLogFileInstant(file: Path, gzip: Boolean)(using ZoneId)
    : IO[Checked[Instant]] =
      Resource.fromAutoCloseable:
        IO.blocking:
          if gzip then
            GZIPInputStream(FileInputStream(file.toFile), FirstChunkSize)
          else
            FileInputStream(file.toFile)
      .use: in =>
        IO.blocking:
          ByteArray.unsafeWrap:
            in.readNBytes(FirstChunkSize)
      .map: chunk =>
        chunk.indexOf('\n') match
          case firstLineEnd if firstLineEnd >= 30 /*minimum length of headline*/ =>
            locally:
              if FastTimestampParser.isHeaderLine(chunk.slice(0, firstLineEnd + 1)) then
                chunk.indexOf('\n', firstLineEnd + 1) match
                  case -1 => Left(IncompleteLogFileProblem(file))
                  case secondLineEnd => Right(chunk.slice(firstLineEnd + 1, secondLineEnd))
              else
                logger.error(s"Missing header line in ${file.getFileName}, check your log4j2.xml!")
                Right(chunk.slice(0, firstLineEnd))
            .flatMap: logLine =>
              // Timestamp of first log line after the header line
              FastTimestampParser()
                .parseTimestampInLogLine(logLine)
                .toOption.toRight:
                  InvalidTimestampInLogFileProblem(file, logLine.utf8StringTruncateAt(30))
                .map(_.toInstant)
          case _ =>
            Left(IncompleteLogFileProblem(file))
      .recover:
        case _: EOFException => Left(IncompleteLogFileProblem(file))
        case _: FileNotFoundException => Left(IncompleteLogFileProblem(file))
        case t =>
          if t.getStackTrace != null then
            logger.debug(s"❓readLogFileInstant ${file.getFileName}: ${t.toStringWithCauses}", t)
          Left(Problem.fromThrowable(t))
  end LogFile


  /** The deferred LogFileIndex and optionally the temporary decompressed file. */
  private final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    fileSize: Option[(original: Long, decompressed: Long)] = None)


  sealed trait LogDirectoryIndexMXBean:
    this: Bean.type =>

    def getTmpFilesSize: Long =
      tmpFilesSize


  object Bean extends LogDirectoryIndexMXBean:
    protected[LogDirectoryIndex] var tmpFilesSize: Long = 0
