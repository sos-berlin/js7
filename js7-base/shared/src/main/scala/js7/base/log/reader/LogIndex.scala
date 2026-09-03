package js7.base.log.reader

import cats.effect.std.Supervisor
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import fs2.concurrent.SignallingRef
import fs2.{Chunk, Stream}
import java.nio.file.{Path, Paths}
import java.time.{Instant, ZoneId}
import java.util.concurrent.ConcurrentSkipListMap
import js7.base.catsutils.CatsEffectExtensions.orThrow
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.bytesToPosAndLines
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.file.watch.DirectoryEvent
import js7.base.log.Logger.syntax.*
import js7.base.log.reader.LogIndex.*
import js7.base.log.reader.LogIndexBuilder.{LogFileAdded, LogFileDeleted, LogFileEvent, LogFileIndexDeleted}
import js7.base.log.reader.LogLineKey
import js7.base.log.reader.recompressors.Recompressor
import js7.base.log.{Logger, reader}
import js7.base.service.Service
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{ConcurrentHashMap, ScalaUtils}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.math.Ordered.orderingToOrdered

/** Provides continuous streams of log lines from a stream of log files.
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
  * @param watchGrowth when growing log files should be respected (uncompressed only)
  */
final class LogIndex private(
  initialFiles: Iterable[LogFile],
  logFileEvents: Stream[IO, LogFileEvent],
  breakLinesLongerThan: Option[Int],
  watchGrowth: Boolean,
  label: String,
  fileAddedSignal: SignallingRef[IO, EpochNano])
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
              var logLine = event.toString
              val replaced = instantToLogFile.put(logFile.fileInstant, logFile)
              Option(replaced).foldMap: replacedLogFile =>
                //?IO.whenA(replacedLogFile.filename != logFile.filename):
                  logLine += s", replace ${replacedLogFile.filename}"
                  fileToInstant.remove(replacedLogFile.filename)
                  replacedLogFile.releaseIndex
              *> IO.defer:
                fileToInstant.put(logFile.filename, logFile.fileInstant)
                logLines += logLine
                fileAddedSignal.set(logFile.fileEpochNano)

        case event @ LogFileDeleted(filename) =>
          IO.uncancelable: _ =>
            var logLine = event.toString
            fileToInstant.remove(filename).foldMap: instant =>
              IO.whenA(Option(instantToLogFile.get(instant)).exists(_.filename == filename)):
                Option(instantToLogFile.remove(instant)).foldMap: logFile =>
                  logLine += s", remove $logFile"
                  logFile.releaseIndex
            .map: _ =>
              logLines += event.toString

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
        fileAddedSignal.waitUntil(_ > lastFileEpochNano)
      *>
        IO:
          Option:
            if forReader.backwards then
              instantToLogFile.lowerEntry(lastFileInstant)
            else
              instantToLogFile.higherEntry(lastFileInstant)
          .map(_.getValue)
    .unNoneTerminate

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
              // TODO Handle incomplete gzip file because it is still being written?
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

  def files: Seq[Path] =
    instantToLogFile.values.asScala.toVector.map(_.originalFile)

  override def toString =
    s"LogIndex($label, ${instantToLogFile.size} files)"


object LogIndex:
  private val logger = Logger[LogIndex]

  /** LogIndex, for initial files and a Stream of DirectoryEvent. */
  private[reader] def directory(
    directory: Path,
    files: Seq[Path],
    directoryEvents: Stream[IO, DirectoryEvent],
    watchGrowth: Boolean,
    label: String)
    (using zoneId: ZoneId, conf: LogIndexConf)
  : ResourceIO[LogIndex] =
    logger.debugResource("LogIndex", s"$directory $label"):
      given Recompressor = conf.recompressor
      for
        (logFiles, pipe) <- LogIndexBuilder.toLogFileEvents(directory, files)
        logFileIndex <- resource(
          logFiles,
          directoryEvents.through(pipe),
          label = label,
          watchGrowth = watchGrowth)
      yield
        logger.whenTraceEnabled:
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
            logger.trace(s"$br$line")
          logFiles
      logIndex <- resource(logFiles, Stream.empty, watchGrowth = watchGrowth, label = label)
    yield
      logIndex

  private def resource(
    initialLogFiles: Iterable[LogFile],
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
          yield
            Service:
              LogIndex(
                initialLogFiles,
                logFileEvents,
                breakLinesLongerThan = Some(summon[LogIndexConf].logFileIndexLineLength),
                watchGrowth = watchGrowth,
                label = label,
                signal)
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
