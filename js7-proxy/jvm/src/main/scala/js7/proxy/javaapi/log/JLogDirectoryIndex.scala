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
    logDirectoryIndex.keyedByteLogLineStream(begin, logSelection.asScala)
      .map(_.lineAsString)
      .asFlux

  def keyedLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[KeyedLogLine] =
    keyedLogLineFlux_(begin, logSelection)

  def keyedLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[KeyedLogLine] =
    keyedLogLineFlux_(begin, logSelection)

  private def keyedLogLineFlux_(begin: Instant | LogLineKey, logSelection: JLogSelection)
  : Flux[KeyedLogLine] =
    logDirectoryIndex.keyedByteLogLineStream(begin, logSelection.asScala)
      .map(_.toKeyedLogLine)
      .asFlux

  def keyedByteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[KeyedByteLogLine] =
    keyedByteLogLineFlux_(begin, logSelection)

  def keyedByteLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[KeyedByteLogLine] =
    keyedByteLogLineFlux_(begin, logSelection)

  private def keyedByteLogLineFlux_(begin: Instant | LogLineKey, logSelection: JLogSelection)
  : Flux[KeyedByteLogLine] =
    logDirectoryIndex.keyedByteLogLineStream(begin, logSelection.asScala)
      .asFlux

  def instantToLogLineKey(instant: Instant, logSelection: JLogSelection)
  : CompletableFuture[Optional[LogLineKey]] =
    logDirectoryIndex.instantToLogLineKey(instant, logSelection.asScala)
      .map(_.toJava)
      .unsafeToCompletableFuture()


object JLogDirectoryIndex:

  /**
    * @param directory Watched directory containing the log files
    * @param isValidFile Which files should be considered
    * @param logLevel Error, Info or Debug
    * @param watchGrowth if growing log files number and size should be watched and indexed
    * @param zoneId
    * @param ctx The runtime
    */
  def directory(
    directory: Path,
    isValidFile: Predicate[Path],
    logLevel: LogLevel,
    watchGrowth: Boolean,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.ioRuntime
    given Config = ctx.config
    given ZoneId = zoneId
    resource_(logLevel):
      LogDirectoryIndex.directory(directory, logLevel,
        watchGrowth = watchGrowth,
        isValidFile.asScala)

  def files(
    files: java.lang.Iterable[Path],
    logLevel: LogLevel,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.ioRuntime
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
