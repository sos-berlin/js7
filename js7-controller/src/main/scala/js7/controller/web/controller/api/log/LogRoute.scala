package js7.controller.web.controller.api.log

import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import java.nio.file.Files.{isReadable, isRegularFile}
import java.nio.file.Path
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.{ChronoField, TemporalQuery}
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId}
import js7.base.auth.ValidUserPermission
import js7.base.configutils.Configs.{ConvertibleConfig, RichConfig}
import js7.base.convert.AsJava.StringAsPath
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.io.file.ByteSeqFileReader
import js7.base.io.file.LogFileReader.growingLogFileStream
import js7.base.log.{LogFileIndex, Logger}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichEither}
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithStream
import js7.common.pekkohttp.PekkoHttpServerUtils.extensions.rechunkToByteStringSporadic
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

trait LogRoute extends ControllerRouteProvider:

  protected def ioRuntime: IORuntime
  protected def config: Config

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
          fileRoute(currentInfoLogFile)
      ~
        pathPrefix("debug"):
          fileRoute(currentDebugLogFile)

  private def fileRoute(logFile: Path): Route =
    path("snapshot"):
      seal:
        streamFile(logFile)
    ~
      pathEnd:
        parameter("start".as[String].?):
          case None =>
            streamFile(logFile, growing = true)

          case Some(startString) =>
            stringToInstant(startString).fold(complete(_), from =>
              section(logFile, from))

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
      snapshot(file)

  private def growingLog(file: Path, pollDuration: FiniteDuration): Route =
    completeWithStream(`text/plain(UTF-8)`):
      growingLogFileStream[fs2.Chunk[Byte]](
        file, byteChunkSize = httpChunkSize, pollDuration, fromEnd = true
      ).rechunkToByteStringSporadic(httpChunkSize)
        .interruptWhenF(shutdownSignaled)

  private def snapshot(file: Path): Route =
    completeWithStream(`text/plain(UTF-8)`):
      ByteSeqFileReader.stream[fs2.Chunk[Byte]](file, byteChunkSize = httpChunkSize)
        .map(_.toByteString)
        .interruptWhenF(shutdownSignaled)

  private def section(logFile: Path, start: Instant): Route =
    parameter("lines".as[Int].?): lineCount =>
      completeWithStream(`text/plain(UTF-8)`):
        fs2.Stream.resource:
          LogFileIndex.resource(logFile)
        .flatMap: logFileIndex =>
          logFileIndex.streamSection(start)
            //.map:
            //  removeHighlights  –– MAKE A FAST VARIANT, or let the client do this slow operation
            .pipeMaybe(lineCount): (stream, n) =>
              stream.take(n)
            .chunks
            .rechunkToByteStringSporadic(httpChunkSize)
            .map(_.toByteString)


object LogRoute:
  private val logger = Logger[LogRoute]
  private val FileChangeWatchPeriod = 5.s

  private val dateTimeFormatter: DateTimeFormatter =
    val base =
      new DateTimeFormatterBuilder()
        .parseCaseSensitive()
        .appendPattern("yyyy-MM-dd")

    def withDateTimeSeparator(sep: Char) =
      new DateTimeFormatterBuilder()
        .append(base.toFormatter)
        .appendLiteral(sep)
        .appendPattern("HH:mm:ss")

    // Build one formatter that accepts either 'T' or ' ' by using optional sections
    new DateTimeFormatterBuilder()
      .parseCaseSensitive()
      .append(withDateTimeSeparator('T').toFormatter)
      // Optional fraction with '.' (3..6 digits)
      .optionalStart()
      .appendLiteral('.')
      .appendFraction(ChronoField.NANO_OF_SECOND, 1, 9, false)
      .optionalEnd()
      // Optional fraction with ',' (3..6 digits)
      .optionalStart()
      .appendLiteral(',')
      .appendFraction(ChronoField.NANO_OF_SECOND, 1, 9, false)
      .optionalEnd()
      // Optional offset, e.g. Z or +02:00
      .optionalStart()
      .appendOffsetId()
      .optionalEnd()
      //// Optional region zone in brackets, e.g. [Europe/Mariehamn]
      //.optionalStart()
      //.appendLiteral('[')
      //.appendZoneRegionId()
      //.appendLiteral(']')
      //.optionalEnd()
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
