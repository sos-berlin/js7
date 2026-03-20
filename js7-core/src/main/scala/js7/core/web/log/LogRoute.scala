package js7.core.web.log

import cats.effect.std.Mutex
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import cats.syntax.apply.*
import cats.syntax.parallel.*
import com.typesafe.config.Config
import fs2.Stream
import java.nio.file.Files.{isReadable, isRegularFile}
import java.nio.file.Path
import java.time
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField.NANO_OF_SECOND
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId}
import js7.base.auth.ValidUserPermission
import js7.base.catsutils.Environment
import js7.base.catsutils.Environment.environment
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig}
import js7.base.convert.AsJava.StringAsPath
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.LogLevel.Debug
import js7.base.log.reader.LogDirectoryIndex.LogDirectoryIndexMXBean
import js7.base.log.reader.LogFileReader.growingLogFileStream
import js7.base.log.reader.{KeyedByteLogLine, LogDirectoryIndex, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.service.Service
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEither, RichEitherF, RichThrowable}
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.pekkohttp.PekkoHttpServerUtils.extensions.{encodeJsonAndRechunkToByteStringSporadic, rechunkToByteStringSporadic}
import js7.common.pekkohttp.PekkoHttpServerUtils.{completeIO, completeWithStream}
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.core.web.log.LogRoute.*
import js7.data.node.EngineServerId
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.PathDirectives.{path, pathEnd}
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

trait LogRoute extends RouteProvider:

  protected def ioRuntime: IORuntime
  protected def config: Config
  protected def engineServerId: IO[Checked[EngineServerId]]
  protected def dataDirectory: Path

  private given IORuntime = ioRuntime

  private lazy val pollDuration =
    config.finiteDuration("js7.web.server.services.log.poll-interval").orThrow

  private lazy val currentInfoLogFile = config.as[Path]("js7.log.info.file")
  private lazy val currentDebugLogFile = config.as[Path]("js7.log.debug.file")

  lazy val logRoute: Route =
    authorizedUser(ValidUserPermission): _ =>
      pathEnd:
        seal:
          streamFile(currentInfoLogFile, growing = true)
      ~
        pathPrefix("info"):
          fileRoute(LogLevel.Info, currentInfoLogFile)
      ~
        pathPrefix("debug"):
          fileRoute(Debug, currentDebugLogFile)
      ~
        pathPrefix("none"):
          // LogLevel.None returns a line for  testing
          completeIO:
            engineServerId.mapmap: engineServerId =>
              s"TEST ONLY: $engineServerId, ${operatingSystem.hostname}\n"

  private def fileRoute(logLevel: LogLevel, currentLogFile: Path): Route =
    path("raw"):
      seal:
        streamFile(currentLogFile)
    ~
      pathEnd:
        parameter("begin".as[String].?):
          case None =>
            streamFile(currentLogFile, growing = true)

          case Some(beginString) =>
            locally:
              if beginString.contains("/") then
                LogLineKey.parse(beginString)
              else
                stringToInstant(beginString)
            .fold(complete(_), begin =>
              section(logLevel, begin))

  private def streamFile(file: Path, growing: Boolean = false): Route =
    //Fails if file grows while being read (Content-Length mismatch?): getFromFile(currentInfoLogFile, contentType)
    if !isRegularFile(file) || !isReadable(file) then
      // TODO Wait for file ???
      // TODO Simply catch FileNotFoundException
      complete(NotFound, "Missing log file")
    else if growing then
      parameter("poll" ? pollDuration): pollDuration_ => // TODO TEST Only
        growingLog(file, pollDuration_ max 10.ms)
    else
      rawFile(file)

  private def growingLog(file: Path, pollDuration: FiniteDuration): Route =
    completeWithStream(`text/plain(UTF-8)`):
      growingLogFileStream[fs2.Chunk[Byte]](
        file, byteChunkSize = httpChunkSize, pollDuration, fromEnd = true
      ).map(_.toByteString)
        .interruptWhenF(shutdownSignaled)

  private def rawFile(file: Path): Route =
    completeWithStream(`text/plain(UTF-8)`):
      ByteSeqFileReader.stream[fs2.Chunk[Byte]](file, byteChunkSize = httpChunkSize)
        .map(_.toByteString)
        .interruptWhenF(shutdownSignaled)

  private def section(logLevel: LogLevel, begin: Instant | LogLineKey): Route =
    parameter("lines".as[Long].?): lineLimit =>
      completeWithStream(`text/plain(UTF-8)`):
        toStream(logLevel, begin)
          .pipeMaybe(lineLimit):
            _.take(_)
          .map(_.byteLine)
          .chunks
          .rechunkToByteStringSporadic(httpChunkSize)
          .map(_.toByteString)
      ~
        completeWithStream(`application/x-ndjson`):
          toStream(logLevel, begin)
            .pipeMaybe(lineLimit):
              _.take(_)
            .map(_.toKeyedLogLine)
            .encodeJsonAndRechunkToByteStringSporadic(httpChunkSize)

  private def toStream(logLevel: LogLevel, begin: Instant | LogLineKey)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      environment[LogDirectoryIndexEnv].flatMap:
        _.forLogLevel(logLevel, config)
    .flatMap: logDirectoryIndex =>
      begin match
        case instant: Instant =>
          logDirectoryIndex.instantToKeyedByteLogLineStream(instant, byteChunkSize = httpChunkSize)
        case logLineKey: LogLineKey =>
          logDirectoryIndex.keyToKeyedByteLogLineStream(logLineKey, byteChunkSize = httpChunkSize)
            .drop(1)

object LogRoute:
  private val logger = Logger[LogRoute]
  private val FileChangeWatchPeriod = 5.s

  private val dateTimeFormatter: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendPattern("yyyy-MM-dd")
      .appendLiteral('T')
      .appendPattern("HH:mm:ss")
      .optionalStart()
      .optionalStart()
      // Optional fraction with .,' (1..9 digits)
      .appendLiteral('.')
      .appendFraction(NANO_OF_SECOND, 1, 9, false)
      .optionalEnd()
      // Optional fraction with ',' (1..9 digits)
      .optionalStart()
      .appendLiteral(',')
      .appendFraction(NANO_OF_SECOND, 1, 9, false)
      .optionalEnd()
      // Optional offset, e.g. Z or +02:00
      .optionalStart()
      .appendOffsetId()
      .optionalEnd()
      .toFormatter()

  private[log] def stringToInstant(string: String): Checked[Instant] =
    catchNonFatalFlatten:
      dateTimeFormatter.parseBest(string,
        //ZonedDateTime.from,
        OffsetDateTime.from,
        LocalDateTime.from)
      match
        //case o: ZonedDateTime => Right(o.toInstant)
        case o: OffsetDateTime => Right(o.toInstant)
        case o: LocalDateTime => Right(o.atZone(ZoneId.systemDefault).toInstant)

  private def relativise(baseDir: Path, targetFile: Path): Path =
    if !baseDir.isAbsolute || !targetFile.isAbsolute then
      targetFile
    else
      try
        baseDir.relativize(targetFile)
      catch case NonFatal(e) =>
        logger.debug(s"❓ relativise: ${e.toStringWithCauses}")
        targetFile


  /** [[LogDirectoryIndex]] placed in the [[Environment]]. */
  final class LogDirectoryIndexEnv private(poll: FiniteDuration, mutex: Mutex[IO])(using ZoneId)
  extends Service.TrivialReleasable:
    private val levelToIndex = mutable.HashMap.empty[LogLevel, Allocated[IO, LogDirectoryIndex]]

    protected def release =
      mutex.lock.surround:
        IO.defer:
          levelToIndex.values.toSeq.parTraverseVoid(_.release)

    def forLogLevel(logLevel: LogLevel, config: Config): IO[LogDirectoryIndex] =
      mutex.lock.surround:
        IO.defer:
          levelToIndex.get(logLevel) match
            case None =>
              LogDirectoryIndex.js7Directory(logLevel, config,
                  poll = Some(poll))
                .toAllocated.map: allocated =>
                  levelToIndex.put(logLevel, allocated)
                  allocated.allocatedThing
            case Some(allocated) =>
              IO.pure(allocated.allocatedThing)

    override def toString = "LogDirectoryIndexEnv"

  object LogDirectoryIndexEnv:
    private given ZoneId = ZoneId.systemDefault

    def register(config: Config): ResourceIO[Unit] =
      val poll = config.finiteDuration("js7.web.server.services.log.poll-interval").orThrow
      Environment.register:
        Service:
          Mutex[IO].map: mutex =>
            new LogDirectoryIndexEnv(poll, mutex)
      *>
        registerStaticMBean[LogDirectoryIndexMXBean]("LogDirectoryIndex", LogDirectoryIndex.Bean)
          .void