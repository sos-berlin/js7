package js7.proxy.javaapi.log

import cats.effect.ResourceIO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.function.Predicate
import js7.base.log.LogLevel
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogDirectoryIndex, LogLineKey}
import js7.data_for_java.reactor.ReactorConverters.asFlux
import js7.proxy.javaapi.{JProxyContext, JResource}
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*
import scala.jdk.FunctionConverters.*
import scala.jdk.OptionConverters.*

final class JLogDirectoryIndex private(logDirectoryIndex: LogDirectoryIndex)(using IORuntime):

  def lineFlux(begin: Instant, logSelection: JLogSelection): Flux[String] =
    logDirectoryIndex.instantToKeyedByteLogLineStream(begin, logSelection.toScala)
      .map(_.lineAsString)
      .asFlux

  def keyedLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[KeyedLogLine] =
    logDirectoryIndex.instantToKeyedByteLogLineStream(begin, logSelection.toScala)
      .map(_.toKeyedLogLine)
      .asFlux

  def keyedByteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[KeyedByteLogLine] =
    logDirectoryIndex.instantToKeyedByteLogLineStream(begin, logSelection.toScala)
      .asFlux

  def instantToLogLineKey(instant: Instant, logSelection: JLogSelection): CompletableFuture[Optional[LogLineKey]] =
    logDirectoryIndex.instantToLogLineKey(instant, logSelection.toScala)
      .map(_.toJava)
      .unsafeToCompletableFuture()


object JLogDirectoryIndex:

  def directory(
    directory: Path,
    isValidFile: Predicate[Path],
    logLevel: LogLevel,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.given_IORuntime
    given Config = ctx.config
    given ZoneId = zoneId
    resource_(logLevel):
      LogDirectoryIndex.directory(directory, logLevel, isValidFile.asScala)

  def files(
    files: java.lang.Iterable[Path],
    logLevel: LogLevel,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.given_IORuntime
    given ZoneId = zoneId
    given Config = ctx.config
    resource_(logLevel):
      LogDirectoryIndex.files(files.asScala, logLevel)

  private def resource_(logLevel: LogLevel)
    (to: => ResourceIO[LogDirectoryIndex])
    (using IORuntime)
  : JResource[JLogDirectoryIndex] =
    JResource:
      to.map:
        JLogDirectoryIndex(_)
