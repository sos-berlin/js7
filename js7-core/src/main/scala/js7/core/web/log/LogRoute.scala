package js7.core.web.log

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import fs2.Stream
import java.nio.file.Path
import java.time.Instant
import java.time.format.DateTimeFormatter.ISO_INSTANT
import java.util.regex.Pattern
import js7.base.auth.ValidUserPermission
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.reader.{KeyedByteLogLine, LogDirectoryIndexRegister, LogLineKey, LogReaders, LogSelection}
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchNonFatal
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.common.pekkohttp.PekkoHttpServerUtils.extensions.{mapAndRechunkToByteStringSporadic, rechunkToByteStringSporadic}
import js7.common.pekkohttp.PekkoHttpServerUtils.{completeIO, completeWithStream}
import js7.common.pekkohttp.StandardMarshallers
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.core.web.log.LogRoute.*
import js7.data.node.GroupAndServerId
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.StatusCodes.Forbidden
import org.apache.pekko.http.scaladsl.model.headers.`User-Agent`
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.PathDirectives.pathEnd
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshaller
import org.apache.pekko.util.ByteString

trait LogRoute extends RouteProvider:

  protected def ioRuntime: IORuntime
  protected def config: Config
  protected def groupAndServerId: Option[GroupAndServerId]
  protected def logDirectory: Path
  protected def logDirectoryIndexRegister: LogDirectoryIndexRegister

  private given IORuntime = ioRuntime

  private lazy val pollDuration =
    config.finiteDuration("js7.web.server.services.log.poll-interval").orThrow
  private lazy val currentInfoLogFilename = config.getString("js7.log.info.file")
  private lazy val currentDebugLogFilename = config.getString("js7.log.debug.file")

  lazy val logRoute: Route =
    authorized(ValidUserPermission):
      pathEnd:
        seal:
          fileRoute(LogLevel.Info)
      ~
        path("error"):
          seal:
            fileRoute(LogLevel.Error)
      ~
        path("info"):
          seal:
            fileRoute(LogLevel.Info)
      ~
        path("debug"):
          seal:
            fileRoute(LogLevel.Debug)
      ~
        path("none"):
          seal:
            // LogLevel.None returns a line for testing
            if !isTest then
              complete(Forbidden)
            else
              completeIO:
                IO:
                  s"TEST ONLY: ${groupAndServerId.fold("No Js7ServerId")(_.serverId)
                  }, ${operatingSystem.hostname}\n"


  private def fileRoute(logLevel: LogLevel): Route =
    import StandardMarshallers.{instantUnmarshaller, patternUnmarshaller}
    (
      parameters(
        "begin".as[String].?,
        "end".as[Instant].?,
        "lineLimit".as[Long].?,
        "pattern".as[Pattern].?,
        "withKey".as[Boolean] ? false,
        "growing".as[Boolean].?
      ) & optionalHeaderValueByType(`User-Agent`)
    ): (beginString, end, lineLimit, pattern, withKey, maybeGrowing, userAgent) =>
      val growing = maybeGrowing.getOrElse(!beginString.isDefined) // Comfort for testing with curl
      beginString.fold(Right(Instant.now)): beginString =>
        if beginString.contains("/") then
          LogLineKey.parse(beginString)
        else
          stringToInstant(beginString)
      .fold(complete(_), begin =>
        completeWithStream(`text/plain(UTF-8)`):
          given Config = config
          val logSelection = LogSelection(
            end = end, lineLimit = lineLimit, pattern = pattern, growing = growing,
            byteChunkSize = httpChunkSize)
          Stream.eval:
            logDirectoryIndexRegister.forLogLevel(logLevel)
          .through: stream =>
            if withKey then
              stream.flatMap:
                _.keyedByteLogLineStream(begin, logSelection)
              .mapAndRechunkToByteStringSporadic(httpChunkSize):
                _.asByteSeq.toByteString
            else
              stream.flatMap:
                _.byteLineStream(begin, logSelection)
              .chunks.rechunkToByteStringSporadic(httpChunkSize)
              .map(_.toByteString)
          // We send heartbeats because recompressing and indexing may take some time.
          .keepAlive(
            if userAgent.exists(_.value.contains("JS7")) then EngineHeartbeatPeriod
            else OtherHeartbeatPeriod,
            IO.pure(LogHeartbeat))
      )

  private def toStream(logLevel: LogLevel, begin: Instant | LogLineKey, logSelection: LogSelection)
  : Stream[IO, KeyedByteLogLine] =
    Stream.eval:
      given Config = config
      logDirectoryIndexRegister.forLogLevel(logLevel)
    .flatMap:
      _.keyedByteLogLineStream(begin, logSelection)


object LogRoute:
  private val logger = Logger[LogRoute]
  private val EngineHeartbeatPeriod = 1.s // Short, to allow allow canceling with Cats Effect & Pekko
  private val OtherHeartbeatPeriod = 10.s // For testing (curl)
  private val LogHeartbeat = ByteString(LogReaders.LogHeartbeat)

  private[log] def stringToInstant(string: String): Checked[Instant] =
    catchNonFatal:
      ISO_INSTANT.parse(string, Instant.from)

  private def relativise(baseDir: Path, targetFile: Path): Path =
    if !baseDir.isAbsolute || !targetFile.isAbsolute then
      targetFile
    else
      try
        baseDir.relativize(targetFile)
      catch case e: IllegalArgumentException =>
        logger.debug(s"❓ relativise: ${e.toStringWithCauses}")
        targetFile
