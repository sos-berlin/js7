package js7.proxy.javaapi.log

import cats.effect.ResourceIO
import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.function.BiPredicate
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.LogLevel
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogDirectoryIndex, LogLineKey}
import js7.data_for_java.reactor.ReactorConverters.asFlux
import js7.proxy.javaapi.{JProxyContext, JResource}
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final class JLogDirectoryIndex private(logDirectoryIndex: LogDirectoryIndex)(using IORuntime):

  def lineFlux(begin: Instant, end: Optional[Instant]): Flux[String] =
    logDirectoryIndex.instantToKeyedByteLogLineStream(begin)
      .map(_.byteLine.utf8String)
      .asFlux

  def keyedLogLineFlux(begin: Instant, end: Optional[Instant]): Flux[KeyedLogLine] =
    logDirectoryIndex.instantToKeyedByteLogLineStream(begin)
      .map(_.toKeyedLogLine)
      .asFlux

  def keyedByteLogLineFlux(begin: Instant, end: Optional[Instant]): Flux[KeyedByteLogLine] =
    logDirectoryIndex.instantToKeyedByteLogLineStream(begin)
      .asFlux

  def instantToLogLineKey(instant: Instant): CompletableFuture[Optional[LogLineKey]] =
    logDirectoryIndex.instantToLogLineKey(instant)
      .map(_.toJava)
      .unsafeToCompletableFuture()


object JLogDirectoryIndex:

  def resource(directory: Path, logLevel: LogLevel, zoneId: ZoneId, ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    resource(directory, LogDirectoryIndex.isValidFile, logLevel, zoneId, ctx)

  def resource(
    directory: Path,
    isValidFile: BiPredicate[Path, LogLevel],
    logLevel: LogLevel,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.given_IORuntime
    resource_(logLevel, zoneId):
      LogDirectoryIndex.
        resource(directory, logLevel, isValidFile = LogDirectoryIndex.isValidFile)(using zoneId)

  def resource(
    files: java.lang.Iterable[Path],
    logLevel: LogLevel,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.given_IORuntime
    resource_(logLevel, zoneId):
      LogDirectoryIndex.resource(files.asScala, logLevel)(using zoneId)

  private def resource_(logLevel: LogLevel, zoneId: ZoneId)
    (to: => ResourceIO[LogDirectoryIndex])
    (using IORuntime)
  : JResource[JLogDirectoryIndex] =
    JResource:
      to.map:
        JLogDirectoryIndex(_)
