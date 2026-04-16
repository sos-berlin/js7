package js7.base.log.reader

import cats.effect.std.AtomicCell
import cats.effect.{IO, Resource, ResourceIO, SyncIO}
import cats.syntax.apply.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import fs2.{Chunk, Stream}
import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.concurrent.ConcurrentSkipListMap
import java.util.zip.GZIPInputStream
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.configutils.Configs.RichConfig
import js7.base.data.ByteArray
import js7.base.fs2utils.ByteChunksLineSplitter.BreakLinesLongerThan
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.inputStreamToStream
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.watch.{DirectoryEvent, DirectoryState, DirectoryWatch, DirectoryWatchSettings}
import js7.base.io.file.{ByteSeqFileReader, FileDeleter}
import js7.base.log.LogLevel.{Debug, Info}
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogDirectoryIndex.*
import js7.base.log.reader.LogFileReader.UniqueHeaderSize
import js7.base.log.reader.recompressors.{DeflaterRecompressor, Recompressor}
import js7.base.log.reader.{LogDirectoryIndex, LogFileIndex, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.service.Service
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.time.EpochNano
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, ConcurrentHashMap, LockKeeper}
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*

/** Provides a continuous stream of log lines over all log files in the directory.
  * @param directoryEvents updates the file list, must emit events only from `logDirectory`
  * @param pollDuration when growing log files should be respected (uncompressed only)
  */
final class LogDirectoryIndex private(
  initialLogFiles: Iterable[LogFile],
  directoryEvents: Stream[IO, OurDirEvent],
  logLevel: LogLevel,
  pollDuration: Option[FiniteDuration],
  recompressor: Recompressor,
  breakLinesLongerThan: Option[Int])
  (using zoneId: ZoneId)
extends Service.StoppableByCancel:

  private val instantToLogFile =
    new ConcurrentSkipListMap(initialLogFiles.toKeyedMap(_.fileInstant).asJava)
  private val fileToInstant: ConcurrentHashMap[Path, Instant] =
    ConcurrentHashMap.from:
      instantToLogFile.asScala.toMap.map((instant, logFile) => logFile.filename -> instant)

  import ByteSeqFileReader.BufferSize as DefaultBufferSize

  protected def start =
    startService:
      run.productR:
        IO.defer:
          instantToLogFile.values.asScala.toVector.parFoldMapA: logFile =>
            logFile.release

  private def run =
    // Cancelled when service is stopping
    directoryEvents.chunks.map: chunk =>
      chunk.asSeq.foreachWithBracket()((evt, br) => logger.debug(s"$br$evt"))
      chunk
    .unchunks
    .evalMap:
      case FileAdded(file) =>
        IO.uncancelable: _ =>
          toLogFile(file).map: maybe =>
            maybe.foreach: logFile =>
              fileToInstant.put(logFile.filename, logFile.fileInstant)
              instantToLogFile.put(logFile.fileInstant, logFile)

      case FileDeleted(file) =>
        IO.uncancelable: _ =>
          fileToInstant.remove(file).foldMap: instant =>
            Option(instantToLogFile.remove(instant)).foldMap: logFile =>
              logger.debug(s"Remove $logFile")
              logFile.release
    .compile.drain

  private def release =
    IO.defer:
      instantToLogFile.values.asScala.toVector.parFoldMapA: logFile =>
        logFile.release

  def instantToLogLineKey(instant: Instant): IO[Option[LogLineKey]] =
    instantToKeyedByteLogLineStream(instant)
      .head.compile.last.map(_.map(_.key))

  def instantToKeyedByteLogLineStream(begin: Instant, byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      if instantToLogFile.isEmpty then
        Stream.empty
      else
        val logFile = instantToLogFile(begin)
        streamAndContinueWithNextFile(logFile, begin, byteChunkSize = byteChunkSize)

  def keyToKeyedByteLogLineStream(logLineKey: LogLineKey, byteChunkSize: Int = DefaultBufferSize)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      if logLineKey.logLevel != logLevel then throw IllegalArgumentException(s"Wrong LogLevel")
      if instantToLogFile.isEmpty then
        Stream.empty
      else
        val logFile = instantToLogFile(logLineKey.fileInstant)
        streamAndContinueWithNextFile(logFile, logLineKey.position, byteChunkSize = byteChunkSize)

  private def instantToLogFile(instant: Instant): LogFile =
    instantToLogFile.floorEntry(instant) match
      case null => instantToLogFile.firstEntry.getValue
      case o => o.getValue

  private def streamAndContinueWithNextFile(logFile: LogFile, instant: Instant, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      toDeferredIndex(logFile)
    .map:
      _.logFileIndex
    .flatMap: logFileIndex =>
      logFileIndex.streamPosAndLine(instant, byteChunkSize = byteChunkSize).map: (pos, line) =>
        KeyedByteLogLine(logFile.toLogLineKey(logLevel, pos), line)
    .append:
      streamSectionAfter(logFile.fileInstant, byteChunkSize = byteChunkSize)

  private def streamAndContinueWithNextFile(logFile: LogFile, position: Long, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      toDeferredIndex(logFile) // todo? Don't build logFile.logFileIndex. Only decompressed file is required
    .flatMap: deferredIndex =>
      Stream.resource:
        ByteSeqFileReader.resource[fs2.Chunk[Byte]](deferredIndex.file, bufferSize = byteChunkSize)
    .flatMap: reader =>
      reader.streamPosAndLines(
        position = position,
        breakLinesLongerThan = breakLinesLongerThan
      ).map: (pos, line) =>
        KeyedByteLogLine(logFile.toLogLineKey(logLevel, pos), line)
    .append:
      streamSectionAfter(logFile.fileInstant, byteChunkSize = byteChunkSize)

  private def streamSectionAfter(instant: Instant, byteChunkSize: Int)
  : Stream[IO, KeyedByteLogLine] =
    Stream.suspend:
      Option:
        instantToLogFile.higherEntry(instant)
      .map(_.getValue).fold(Stream.empty): logFile =>
        streamAndContinueWithNextFile(logFile, instant, byteChunkSize = byteChunkSize)

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
            // Happens when a compressed file has been indexed but decompression hasn't finished
            logger.debug:
              s"${logFile.originalFile.getFileName} changed its size — rebuilding index"
            allo.release *> buildIndex(logFile).toAllocated
          else
            IO.pure(allo)
      .map(Some(_))
    .map(_.get.allocatedThing)

  private def buildIndex(logFile: LogFile): ResourceIO[DeferredIndex] =
    if logFile.isGzipped then
      decompressAndBuildIndex(logFile.originalFile)
    else
      buildIndexFromUncompressedFile(logFile.originalFile)

  private def buildIndexFromUncompressedFile(file: Path): ResourceIO[DeferredIndex] =
    pollDuration match
      case None =>
        Resource.eval:
          LogFileIndex.fromFile(file).map: logFileIndex =>
            DeferredIndex(logFileIndex, file)
      case Some(poll) =>
        LogFileIndex.buildGrowing(file, poll = poll).map: logFileIndex =>
          DeferredIndex(logFileIndex, file)

  private def decompressAndBuildIndex(gzFile: Path): ResourceIO[DeferredIndex] =
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
                toBuilderStream = toDecompressingStream(gzFile, _, recompressor),
                toPositionedStream = positionedTmpFileStream(tmpFile, _, _, recompressor),
                logWriter = recompressor.toLogWriter(tmpFile)
              ).map: logFileIndex =>
                logger.info(s"${recompressor.label} ${gzFile.getFileName}: ${
                  bytesPerSecondString(t.elapsed, logFileIndex.byteCount)}")
                logFileIndex)(
            release = _ =>
              IO.blocking:
                FileDeleter.tryDeleteFile(tmpFile))
        .map: logFileIndex =>
          val recompressedSize = Files.size(tmpFile)
          Bean.tmpFilesSize += recompressedSize
          DeferredIndex(logFileIndex, tmpFile, Some(size -> recompressedSize))

  def files: Seq[Path] =
    instantToLogFile.values.asScala.toVector.map(_.originalFile)

  override def toString =
    s"LogDirectoryIndex($logLevel, ${instantToLogFile.size} files)"


object LogDirectoryIndex:
  private val logger = Logger[LogDirectoryIndex]
  private val TmpSuffix = "-indexed.tmp"
  private val LogGzTmpSuffix = ".log.gz" + TmpSuffix

  /** LogDirectoryIndex, watching the standard JS7 Engine log directory. */
  private def js7Directory(logDirectory: Path, logLevel: LogLevel, config: Config)(using ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(logLevel == Info || logLevel == Debug)
    val poll = config.finiteDuration("js7.web.server.services.log.poll-interval").orThrow
    val currentLogFile = logDirectory / config.getString:
      if logLevel == Debug then "js7.log.debug.file" else "js7.log.info.file"
    val currentLogFilename = currentLogFile.getFileName

    def isValidFile(file: Path) =
      val filename = file.getFileName
      val name = filename.toString
      filename == currentLogFilename ||
        !name.startsWith("~") &&
          !name.endsWith("~") &&
          logLevelMatches(file, logLevel) &&
          isCompressedLogFile(file)

    directory(logDirectory, logLevel, isValidFile, config, Some(poll))
  end js7Directory

  /** LogDirectoryIndex, watching a directory. */
  def directory(
    directory: Path,
    logLevel: LogLevel,
    isValidFile: Path => Boolean,
    config: Config,
    poll: Option[FiniteDuration] = None)
    (using ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    assertThat(logLevel == Info || logLevel == Debug)
    logger.traceResource:
      Resource.suspend:
        val recompressor = Recompressor.fromConfig(config)
        deleteTmpFiles(directory, logLevel) *>
          directory.directoryStream[IO]
            .filter: file =>
              !isOurTmpFile(file) && isValidFile(file)
            .compile.toVector.map: files =>
              val directoryEvents = DirectoryWatch.stream(
                directory,
                DirectoryState(files.map(_.getFileName)),
                DirectoryWatchSettings.fromConfig(config).orThrow,
                isValidFile,
                Set(ENTRY_CREATE, ENTRY_DELETE))
              resource(files, directory, directoryEvents, logLevel, poll, recompressor)

  def files(files: Iterable[Path], logLevel: LogLevel, poll: Option[FiniteDuration] = None)
    (using ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    resource(files, Paths.get(".")/*not used*/, Stream.empty, logLevel, poll, DeflaterRecompressor)

  private def resource(
    files: Iterable[Path],
    logDirectory: Path,
    directoryEvents: Stream[IO, DirectoryEvent],
    logLevel: LogLevel,
    poll: Option[FiniteDuration],
    recompressor: Recompressor)
    (using ZoneId)
  : ResourceIO[LogDirectoryIndex] =
    Resource.suspend:
      toLogFiles(files).map: logFiles =>
        Service:
          val ourDirEvents = directoryEvents
            .filterNot(o => isOurTmpFile(o.relativePath)) // to be sure
            .map:
              case DirectoryEvent.FileAdded(file) => FileAdded(logDirectory.resolve(file))
              case DirectoryEvent.FileDeleted(file) => FileDeleted(logDirectory.resolve(file))
              case o: DirectoryEvent.FileModified => sys.error(s"LogDirectoryIndex: unexpected $o")
          new LogDirectoryIndex(logFiles, ourDirEvents, logLevel, poll, recompressor,
            breakLinesLongerThan = Some(BreakLinesLongerThan))

  /** Extract the timestamp of the first line of each file and return a sequence of [[LogFile]].
    */
  private def toLogFiles(files: Iterable[Path])(using ZoneId): IO[Vector[LogFile]] =
    fs2.Stream.iterable(files)
      .parEvalMapUnordered(sys.runtime.availableProcessors): file =>
        toLogFile(file)
      .compile.toVector
      .map(_.flatten)

  /** Extract the timestamp of the first line of a log file and return a [[LogFile]].
    */
  private def toLogFile(file: Path)(using zoneId: ZoneId): IO[Option[LogFile]] =
    AtomicCell[IO].of(none[Allocated[IO, DeferredIndex]]).flatMap: cell =>
      val size = Files.size(file)
      locally:
        if file.getFileName.toString.endsWith(".gz") then
          readLogFileInstant:
            IO:
              new GZIPInputStream(new FileInputStream(file.toFile), 512)
          .map: maybeInstant =>
            maybeInstant.fold(none): instant =>
              Some(LogFile(instant, file, cell, isGzipped = true))
        else
          readLogFileInstant:
            IO:
              new BufferedInputStream(new FileInputStream(file.toFile), UniqueHeaderSize)
          .flatMap:
            _.traverse: instant =>
              LogFileIndex.fromFile(file).map: logFileIndex =>
                LogFile(instant, file, cell)
      .map: maybe =>
        maybe.foreach: logFile =>
          logger.debug(s"$logFile size=$size")
        maybe
    .handleError: throwable =>
      logger.debug(s"toLogFile ${file.getFileName}: 💥 ${throwable.toStringWithCauses}",
        throwable.nullIfNoStackTrace)
      None

  private def readLogFileInstant(inputStream: IO[InputStream])(using ZoneId): IO[Option[Instant]] =
    Resource.fromAutoCloseable(inputStream).use: in =>
      val array = new Array[Byte](LogFileReader.UniqueHeaderSize)
      IO.blocking:
        ByteArray.unsafeWrap(in.readNBytes(UniqueHeaderSize))
      .map: byteArray =>
        byteArray.length == LogFileReader.UniqueHeaderSize thenMaybe :
          LogFileReader.parseTimestampInHeaderLine(byteArray.utf8String) match
            case EpochNano.Nix => None
            case epochNano => Some(epochNano.toInstant)

  private def deleteTmpFiles(directory: Path, logLevel: LogLevel): IO[Unit] =
    IO.interruptible:
      FileDeleter.tryDeleteFiles:
        directory.directoryStream[SyncIO]
          .filter: file =>
            isOurTmpFile(file) && logLevelMatches(file, logLevel)
          .compile.toVector
          .run()

  private def logLevelMatches(file: Path, logLevel: LogLevel): Boolean =
    (logLevel == Debug) == file.getFileName.toString.contains("-debug")

  private def isCompressedLogFile(file: Path): Boolean =
    file.toString.endsWith(".log.gz")

  private def isOurTmpFile(file: Path): Boolean =
    file.toString.endsWith(LogGzTmpSuffix)

  private def toDecompressingStream(gzFile: Path, bufferSize: Int, recompressor: Recompressor)
  : Stream[IO, Chunk[Byte]] =
    Stream.resource:
      Resource.fromAutoCloseable:
        IO.blocking:
          new GZIPInputStream(
            new FileInputStream(gzFile.toFile),
            bufferSize / 4 /*compression ratio*/)
    .flatMap: in =>
      inputStreamToStream(in, bufferSize = bufferSize)

  private def positionedTmpFileStream(
    file: Path,
    position: Long,
    bufferSize: Int,
    recompressor: Recompressor)
  : Stream[IO, Chunk[Byte]] =
    Stream.resource:
      Resource.fromAutoCloseable:
        IO.blocking:
          new FileInputStream(file.toFile)
    .evalMap: (in: FileInputStream) =>
      IO.blocking:
        in.skip(position)
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

    def release: IO[Unit] =
      deferredIndexCell.getAndSet(None).flatMap:
        _.foldMap: allo =>
          allo.allocatedThing.fileSize.foreach: o =>
            Bean.tmpFilesSize -= o.decompressed
          allo.release

    def toLogLineKey(logLevel: LogLevel, position: Long): LogLineKey =
      LogLineKey(logLevel, fileInstant, position)

    override def toString =
      f"LogFile(${fileInstant.atZone(zoneId).toOffsetDateTime} -> $filename)"


  /** The deferred LogFileIndex and optionally the temporary decompressed file. */
  private final case class DeferredIndex(
    logFileIndex: LogFileIndex,
    file: Path,
    fileSize: Option[(original: Long, decompressed: Long)] = None)


  private sealed trait OurDirEvent:
    def file: Path

  private final case class FileAdded(file: Path) extends OurDirEvent:
    override def toString = s"FileAdded(${file.getFileName})"

  private final case class FileDeleted(file: Path) extends OurDirEvent:
    override def toString = s"FileDeleted(${file.getFileName})"


  final class Register private(logDirectory: Path)(using ZoneId) extends Service.TrivialReleasable:
    private val levelToIndex = ConcurrentHashMap[LogLevel, Allocated[IO, LogDirectoryIndex]]
    private val lock = LockKeeper[LogLevel]()

    protected def release =
      IO.defer:
        levelToIndex.values.toSeq.parTraverseVoid(_.release)

    def forLogLevel(logLevel: LogLevel, config: Config): IO[LogDirectoryIndex] =
      lock.lock(logLevel):
        IO.defer:
          levelToIndex.get(logLevel) match
            case None =>
              js7Directory(logDirectory, logLevel, config)
                .toAllocated.map: allocated =>
                  levelToIndex.put(logLevel, allocated)
                  allocated.allocatedThing
            case Some(allocated) =>
              IO.pure(allocated.allocatedThing)

    override def toString = "Register"

  object Register:
    private given ZoneId = ZoneId.systemDefault

    def resource(logDirectory: Path): ResourceIO[Register] =
      registerStaticMBean[LogDirectoryIndexMXBean]("LogDirectoryIndex", Bean)
        .void
      *>
        Service(new Register(logDirectory))


  sealed trait LogDirectoryIndexMXBean:
    this: Bean.type =>

    def getTmpFilesSize: Long =
      tmpFilesSize

  object Bean extends LogDirectoryIndexMXBean:
    protected[LogDirectoryIndex] var tmpFilesSize: Long = 0
