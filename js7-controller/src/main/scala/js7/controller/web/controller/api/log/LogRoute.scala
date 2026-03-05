package js7.controller.web.controller.api.log

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import fs2.Stream
import java.nio.file.Files.{isReadable, isRegularFile}
import java.nio.file.Path
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField.NANO_OF_SECOND
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId}
import js7.base.auth.ValidUserPermission
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig}
import js7.base.convert.AsJava.StringAsPath
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.reader.LogFileReader.growingLogFileStream
import js7.base.log.reader.{KeyedByteLogLine, LogDirectoryIndex, LogLineKey}
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEither, RichThrowable}
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithStream
import js7.common.pekkohttp.PekkoHttpServerUtils.extensions.{encodeJsonAndRechunkToByteStringSporadic, rechunkToByteStringSporadic}
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.log.LogRoute.*
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.PathDirectives.{path, pathEnd}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

trait LogRoute extends ControllerRouteProvider:

  protected def ioRuntime: IORuntime
  protected def config: Config
  protected def dataDirectory: Path

  private given IORuntime = ioRuntime

  private lazy val pollDuration =
    config.finiteDuration("js7.web.server.services.log.poll-interval").orThrow

  private lazy val currentInfoLogFile = config.as[Path]("js7.log.info.file")
  private lazy val currentDebugLogFile = config.as[Path]("js7.log.debug.file")
  private lazy val logDirectory = currentInfoLogFile.getParent

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
          fileRoute(LogLevel.Debug, currentDebugLogFile)

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
    Stream.resource:
      LogDirectoryIndex.resource(logDirectory, logLevel)(using ZoneId.systemDefault)
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
