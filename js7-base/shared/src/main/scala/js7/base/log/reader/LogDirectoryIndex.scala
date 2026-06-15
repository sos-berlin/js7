package js7.base.log.reader

import cats.effect.std.AtomicCell
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Stream}
import java.io.{FileInputStream, InputStream}
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.concurrent.ConcurrentSkipListMap
import java.util.zip.GZIPInputStream
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.catsutils.Environment.environment
import js7.base.config.Js7Conf
import js7.base.configutils.Configs.RichConfig
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.{inputStreamToStream, toPosAndLines}
import js7.base.io.OpaquePos
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryEvent, DirectoryState, DirectoryWatch, DirectoryWatchSettings}
import js7.base.io.file.{ByteSeqFileReader, FileDeleter}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogFileUtils.applyLogSelection
import js7.base.log.reader.recompressors.Recompressor
import js7.base.log.reader.{LogFileIndex, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ConcurrentHashMap}
import scala.concurrent.duration.Deadline
import scala.jdk.CollectionConverters.*
import scala.math.Ordered.orderingToOrdered
import scala.util.Try

/** Provides a continuous stream of log lines over all log files in the directory.
  * @param directoryEvents updates the file list, must emit events only from `directory`
  * @param watchGrowth when growing log files should be respected (uncompressed only)
  */
final class LogDirectoryIndex private(
  initialFiles: Iterable[LogFile],
  directoryEvents: Stream[IO, OurDirEvent],
  logLevel: LogLevel,
  recompressor: Recompressor,
  breakLinesLongerThan: Option[Int],
  watchGrowth: Boolean,
  fileAddedSignal: SignallingRef[IO, EpochNano])
  (using zoneId: ZoneId, config: Config)
extends Service.StoppableByCancel:

  private val instantToLogFile =
    ConcurrentSkipListMap(initialFiles.toKeyedMap(_.fileInstant).asJava)
  private val fileToInstant: ConcurrentHashMap[Path, Instant] =
    ConcurrentHashMap.from:
      instantToLogFile.asScala.toMap.map((instant, logFile) => logFile.filename -> instant)
  private val pollGrowing = config.finiteDuration("js7.log.poll-growing").orThrow

  protected def start =
    startService:
      run.guarantee:
        release

  private def run =
    // Cancelled when service is stopping
    directoryEvents.mapChunks: chunk =>
      chunk.asSeq.foreachWithBracket()((evt, br) => logger.debug(s"$br$evt"))
      chunk
    .evalMap:
      case FileAdded(file) =>
        IO.uncancelable: _ =>
          toLogFile(file).flatMap:
            case None => IO.unit
            case Some(logFile) =>
              fileToInstant.put(logFile.filename, logFile.fileInstant)
              instantToLogFile.put(logFile.fileInstant, logFile)
              fileAddedSignal.set(logFile.fileEpochNano)

      case FileDeleted(file) =>
        IO.uncancelable: _ =>
          fileToInstant.remove(file.getFileName).foldMap: instant =>
            Option(instantToLogFile.remove(instant)).foldMap: logFile =>
              logger.debug(s"Remove $logFile")
              logFile.release
    .compile.drain

  private def release =
    IO.defer:
      instantToLogFile.values.asScala.toVector.parFoldMapA: logFile =>
        logFile.release

  /** Efficient Stream of byte line starting with `begin`.
    *
    * Only the first file will be recompressed and index (if not yet happened).
    */
  def byteLineStream(begin: Instant | LogLineKey, logSelection: LogSelection)
  : Stream[IO, fs2.Chunk[Byte]] =
    val forReader = logSelection.forReader
    begin.match
      case begin: Instant if Option(instantToLogFile.firstKey).forall(begin < _) =>
        // If `begin` is less than the Instant of the first log file, then we don't need to
        // recompress and index. We simply read sequentially, starting with the first log file.
        Option(instantToLogFile.firstEntry).map(_.getValue).fold(Stream.empty): logFile =>
          wholeFile(logFile, forReader) ++
            nextFilesToStream(logFile.fileInstant, forReader)

      case begin =>
        fileToKeyedByteLogLines(begin, forReader)
          .flatMap: (logFile, stream) =>
            stream.map(_.posAndLine) ++
              nextFilesToStream(logFile.fileInstant, forReader)
    .through:
      applyLogSelection(logSelection)
    .map:
      _.byteLine

  private def nextFilesToStream(lastFileInstant: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    Stream.eval:
      IO.whenA(watchGrowth && forReader.growing): // Wait until next file exists
        val lastFileEpochNano = lastFileInstant.toEpochNano
        fileAddedSignal.waitUntil(_ > lastFileEpochNano)
      *>
        IO:
          Option:
            instantToLogFile.higherEntry(lastFileInstant)
          .map(_.getValue)
    .unNoneTerminate
    .flatMap: logFile =>
      wholeFile(logFile, forReader) ++
        nextFilesToStream(logFile.fileInstant, forReader) // asynchronously tail-recursive

  private def nextFilesToStream(logFile: LogFile, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    wholeFile(logFile, forReader) ++
      nextFilesToStream(logFile.fileInstant, forReader)

  private def wholeFile(logFile: LogFile, forReader: LogSelection.ForReader)
  : Stream[IO, PosAndLine] =
    locally:
      if logFile.isGzipped then
        // TODO Handle incomplete gzip file because it is still being written?
        toGzipDecompressingStream(logFile.originalFile, forReader.byteChunkSize)
      else if forReader.growing then
        LogFileReader.streamGrowingLogFile(
          logFile.originalFile,
          byteChunkSize = forReader.byteChunkSize,
          poll = pollGrowing)
      else
        ByteSeqFileReader.stream(logFile.originalFile, byteChunkSize = forReader.byteChunkSize)
    .through:
      toPosAndLines(firstPosition = 0, breakLinesLongerThan = breakLinesLongerThan)
    .map:
      PosAndLine.fromPair

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
    fileToKeyedByteLogLines(begin, logSelection.forReader)
      .flatMap: (logFile, stream) =>
        stream ++
          nextFilesToKeyedLogLines(logFile.fileInstant, logSelection.forReader)
      .through:
        applyLogSelection(logSelection)

  private def fileToKeyedByteLogLines(begin: Instant | LogLineKey, forReader: LogSelection.ForReader)
  : Stream[IO, (LogFile, Stream[IO, KeyedByteLogLine])] =
    begin match
      case begin: Instant =>
        Stream.suspend:
          instantToLogFile(begin).fold(Stream.empty): logFile =>
            Stream.emit:
              logFile -> fileToKeyedByteLogLines(logFile, begin, forReader)

      case LogLineKey(logLevel, fileInstant, position) =>
        Stream.suspend:
          if logLevel != this.logLevel then
            Stream.raiseError(IllegalArgumentException("Wrong LogLevel"))
          else
            instantToLogFile(fileInstant).fold(Stream.empty): logFile =>
              Stream.emit:
                logFile -> fileToKeyedByteLogLines(logFile, position, forReader)

  /** @return None if instantToLogFile is empty. */
  private def instantToLogFile(instant: Instant): Option[LogFile] =
    instantToLogFile.floorEntry(instant) match
      case null => Option(instantToLogFile.firstEntry).map(_.getValue)
      case o => Some(o.getValue)

  private def fileToKeyedByteLogLines(
    logFile: LogFile, position: Long, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      // We must recompress (and not necessarily index) to return positions of the recompressed file
      toDeferredIndex(logFile)
    .flatMap: deferredIndex =>
      if logFile.isGzipped then
        // LogLineIndex converts the (uncompressed) position into an OpaquePos in the compressed file
        deferredIndex.logFileIndex.positionToLines(position, forReader)
      else
        locally:
          if forReader.growing then
            LogFileReader.streamGrowingLogFile(
              deferredIndex.file,
              byteChunkSize = forReader.byteChunkSize,
              poll = pollGrowing,
              position = position)
          else
            ByteSeqFileReader.streamFromPosition[fs2.Chunk[Byte]](
              deferredIndex.file,
              position = position,
              byteChunkSize = forReader.byteChunkSize)
        .through:
          toPosAndLines(firstPosition = position, breakLinesLongerThan = breakLinesLongerThan)
        .map:
          PosAndLine.fromPair
    .map: posAndLine =>
      toKeyedByteLogLine(logFile.fileInstant, posAndLine)

  private def fileToKeyedByteLogLines(
    logFile: LogFile, begin: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      toDeferredIndex(logFile)
    .map:
      _.logFileIndex
    .flatMap: logFileIndex =>
      logFileIndex.streamPosAndLine(begin, forReader)
    .map: posAndLine =>
      toKeyedByteLogLine(logFile.fileInstant, posAndLine)

  // TODO Similar to nextFilesToStream
  private def nextFilesToKeyedLogLines(lastFileInstant: Instant, forReader: LogSelection.ForReader)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      IO.whenA(watchGrowth && forReader.growing):
        val lastFileEpochNano = lastFileInstant.toEpochNano
        fileAddedSignal.waitUntil(_ > lastFileEpochNano)
      *>
        IO:
          logger.trace(s"nextFilesToKeyedLogLines $lastFileInstant")
          Option:
            instantToLogFile.higherEntry(lastFileInstant)
          .map(_.getValue)
    .unNoneTerminate
    .flatMap: logFile =>
      wholeFile(logFile, forReader).map:
        toKeyedByteLogLine(logFile.fileInstant, _)
      .append:
        nextFilesToKeyedLogLines(logFile.fileInstant, forReader)

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
      LogFileIndex.buildGrowing(file, poll = pollGrowing).map: logFileIndex =>
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
  private val TmpSuffix = "-indexed.tmp"
  private val LogGzTmpSuffix = ".log.gz" + TmpSuffix
  /** First chunk of log file must include the timestamp of the second line
    * (the line after the header) */
  private val FirstChunkSize = 1024
  val LogLevels = Set(LogLevel.Error, LogLevel.Info, LogLevel.Debug)

  /** LogDirectoryIndex, watching a directory. */
  def directory(
    directory: Path,
    logLevel: LogLevel,
    watchGrowth: Boolean,
    isValidFile: Path => Boolean)
    (using ZoneId, Config)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(LogLevels(logLevel))
    Resource.suspend:
      watchDirectory(directory, isValidFile).map: (files, directoryEvents) =>
       this.directory(directory, logLevel, files, directoryEvents, watchGrowth = watchGrowth)

  /** LogDirectoryIndex, watching a directory. */
  private[reader] def directory(
    directory: Path,
    logLevel: LogLevel,
    files: Seq[Path],
    directoryEvents: Stream[IO, DirectoryEvent],
    watchGrowth: Boolean)
    (using zoneId: ZoneId, config: Config)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(LogLevels(logLevel))
    logger.traceResource("directory", (directory, logLevel)):
      Resource.suspend:
        deleteTmpFiles(directory, logLevel).map: _ =>
          val recompressor = Recompressor.fromConfig(config)
          resource(files, directory, directoryEvents, logLevel, watchGrowth = watchGrowth,
            recompressor)

  private[reader] def watchDirectory(directory: Path, isValidFile: Path => Boolean)
    (using config: Config)
  : IO[(Vector[Path], Stream[IO, DirectoryEvent])] =
    directory.directoryStream[IO]
      .filter: file =>
        !isOurTmpFile(file) && isValidFile(file)
      .compile.toVector.map: files =>
        files ->
          DirectoryWatch.stream(
            directory,
            DirectoryState(files.map(_.getFileName)),
            DirectoryWatchSettings.fromConfig(config).orThrow,
            isValidFile,
            Set(ENTRY_CREATE, ENTRY_DELETE))

  def files(files: Iterable[Path], logLevel: LogLevel, watchGrowth: Boolean = false)
    (using zoneId: ZoneId, config: Config)
  : ResourceIO[LogDirectoryIndex] =
    resource(files, Paths.get(".")/*not used*/, Stream.empty, logLevel, watchGrowth = watchGrowth,
      Recompressor.fromConfig(config))

  private def resource(
    files: Iterable[Path],
    directory: Path,
    directoryEvents: Stream[IO, DirectoryEvent],
    logLevel: LogLevel,
    watchGrowth: Boolean,
    recompressor: Recompressor)
    (using ZoneId, Config)
  : ResourceIO[LogDirectoryIndex] =
    Resource.suspend:
      for
        js7Conf <- environment[Js7Conf]
        logFiles <- toLogFiles(files)
        signal <- SignallingRef[IO, EpochNano]:
          logFiles.map(_.fileEpochNano).maxOption getOrElse EpochNano.MinValue
      yield
        Service:
          val ourDirEvents = directoryEvents.map(OurDirEvent(directory, _))
          LogDirectoryIndex(logFiles, ourDirEvents, logLevel, recompressor,
            breakLinesLongerThan = Some(js7Conf.logFileIndexLineLength),
            watchGrowth = watchGrowth,
            signal)

  /** Extract the timestamp of the first line of each file and return a sequence of [[LogFile]].
    */
  private def toLogFiles(files: Iterable[Path])(using ZoneId): IO[Vector[LogFile]] =
    fs2.Stream.iterable(files)
      .parEvalMapUnordered(sys.runtime.availableProcessors max 2):
        toLogFile
      .compile.toVector
      .map(_.flatten)

  /** Extract the timestamp of the first line of a log file and return a [[LogFile]].
    */
  private def toLogFile(file: Path)(using ZoneId): IO[Option[LogFile]] =
    AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).flatMap: cell =>
      if file.getFileName.toString.endsWith(".gz") then
        readLogFileInstant(GZIPInputStream(FileInputStream(file.toFile), 8192))
          .map(_.map: instant =>
            LogFile(instant, file, cell, isGzipped = true))
      else
        readLogFileInstant(FileInputStream(file.toFile))
          .flatMap:
            _.traverse: instant =>
              LogFileIndex.fromFile(file).map: logFileIndex =>
                LogFile(instant, file, cell)
    .map: o =>
      o.foreach: logFile =>
        logger.debug(s"$logFile size=${Try(toKBGB(Files.size(file))).fold(identity, identity)}")
      o
    .handleError: throwable =>
      logger.warn(s"toLogFile $file: ${throwable.toStringWithCauses}", throwable.nullIfNoStackTrace)
      None

  private def readLogFileInstant(inputStream: => InputStream)(using ZoneId): IO[Option[Instant]] =
    Resource.fromAutoCloseable(IO(inputStream)).use: in =>
      // TODO Retry when java.io.EOFException: Unexpected end of ZLIB input stream
      IO.blocking:
        ByteArray.unsafeWrap(in.readNBytes(FirstChunkSize))
      .map: chunk =>
        chunk.indexOf('\n') match
          case -1 => None
          case lf =>
            // Timestamp of first log line after the header line
            FastTimestampParser().parseTimestampInLogLine(chunk.drop(lf + 1))
              .toOption.map(_.toInstant)

  private def deleteTmpFiles(directory: Path, logLevel: LogLevel): IO[Unit] =
    IO.blocking:
      FileDeleter.tryDeleteFiles:
        directory.directoryStream[SyncIO]
          .filter: file =>
            isOurTmpFile(file) && fileToLogLevel(file) == logLevel
          .compile.toVector
          .run()

  private[reader] def fileToLogLevel(file: Path): LogLevel =
    if file.getFileName.toString.contains("-error") then
      LogLevel.Error
    else if file.getFileName.toString.contains("-debug") then
      LogLevel.Debug
    else
      LogLevel.Info

  private[reader] def isOurTmpFile(file: Path): Boolean =
    file.toString.endsWith(LogGzTmpSuffix)

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

  /** Description of a log file and a deferred `LogFileIndex`. */
  private final case class LogFile(
    fileInstant: Instant,
    originalFile: Path,
    deferredIndexCell: AtomicCell[IO, Option[Allocated[IO, DeferredIndex]]],
    isGzipped: Boolean = false)
    (using zoneId: ZoneId):

    val filename: Path =
      originalFile.getFileName

    // TODO Prefer fileEpochNano over fileInstant
    val fileEpochNano: EpochNano =
      fileInstant.toEpochNano

    def release: IO[Unit] =
      deferredIndexCell.getAndSet(None).flatMap:
        _.foldMap: allo =>
          allo.allocatedThing.fileSize.foreach: o =>
            Bean.tmpFilesSize -= o.decompressed
          if allo.allocatedThing.file != originalFile then
            FileDeleter.tryDeleteFile(allo.allocatedThing.file)
          allo.release

    override def toString =
      f"LogFile(${fileInstant.atZone(zoneId).toOffsetDateTime} -> $filename)"


  /** The deferred LogFileIndex and optionally the temporary decompressed file. */
  private final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    fileSize: Option[(original: Long, decompressed: Long)] = None)


  private sealed trait OurDirEvent:
    def file: Path

  private object OurDirEvent:
    def apply(directory: Path, event: DirectoryEvent): OurDirEvent =
      event match
        case DirectoryEvent.FileAdded(file) => FileAdded(directory.resolve(file))
        case DirectoryEvent.FileDeleted(file) => FileDeleted(directory.resolve(file))
        case o: DirectoryEvent.FileModified => sys.error(s"LogDirectoryIndex: unexpected $o")


  private final case class FileAdded(file: Path) extends OurDirEvent:
    override def toString = s"FileAdded(${file.getFileName})"

  private final case class FileDeleted(file: Path) extends OurDirEvent:
    override def toString = s"FileDeleted(${file.getFileName})"


  sealed trait LogDirectoryIndexMXBean:
    this: Bean.type =>

    def getTmpFilesSize: Long =
      tmpFilesSize


  object Bean extends LogDirectoryIndexMXBean:
    protected[LogDirectoryIndex] var tmpFilesSize: Long = 0
