package js7.proxy.javaapi.log

import cats.effect.ResourceIO
import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.concurrent.CompletableFuture
import java.util.{Optional, List as JList}
import js7.base.log.LogLevel
import js7.base.log.reader.recompressors.LogFileIndexConf
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogStreamIndex, LogLineKey}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data_for_java.reactor.ReactorConverters.asFluxChunks
import js7.proxy.javaapi.{JProxyContext, JResource}
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final class JLogDirectoryIndex private(logStreamIndex: LogStreamIndex)(using IORuntime)
extends JLogIndex:

  def byteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[Array[Byte]]] =
    logStreamIndex.byteLineStream(begin, logSelection.asScala)
      .map(_.toArray)
      .asFluxChunks

  def byteLogLineFlux(key: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[Array[Byte]]] =
    logStreamIndex.byteLineStream(key, logSelection.asScala)
      .map(_.toArray)
      .asFluxChunks

  def stringLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[String]] =
    logStreamIndex.stringLineStream(begin, logSelection.asScala)
      .asFluxChunks

  def keyedByteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]] =
    keyedByteLogLineFlux_(begin, logSelection)

  def keyedByteLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]] =
    keyedByteLogLineFlux_(begin, logSelection)

  def keyedLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[KeyedLogLine]] =
    keyedLogLineFlux_(begin, logSelection)

  def keyedLogLineFlux(begin: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedLogLine]] =
    keyedLogLineFlux_(begin, logSelection)

  private def keyedByteLogLineFlux_(begin: Instant | LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedByteLogLine]] =
    logStreamIndex.keyedByteLogLineStream(begin, logSelection.asScala)
      .asFluxChunks

  private def keyedLogLineFlux_(begin: Instant | LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedLogLine]] =
    logStreamIndex.keyedByteLogLineStream(begin, logSelection.asScala)
      .map(_.toKeyedLogLine)
      .asFluxChunks

  def instantToLogLineKey(instant: Instant, logSelection: JLogSelection)
  : CompletableFuture[Optional[LogLineKey]] =
    logStreamIndex.instantToLogLineKey(instant, logSelection.asScala)
      .map(_.toJava)
      .unsafeToCompletableFuture()


object JLogDirectoryIndex:

  /** Provides the log files of a living log directory.
    *
    * @param directory Watched directory containing the log files
    * @param filenamePrefix Log file name prefix, for example "joc" (as in joc.log)
    * @param logLevel Error, Info or Debug
    * @param watchGrowth if growing log files number and size should be watched and indexed
    * @param zoneId normally ZoneId.systemDefault
    * @param ctx The runtime
    */
  def directory(
    directory: Path,
    filenamePrefix: String,
    logLevel: LogLevel,
    watchGrowth: Boolean,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.ioRuntime
    given ZoneId = zoneId
    resource_(logLevel):
      for
        given LogFileIndexConf = LogFileIndexConf.fromConfig(ctx.config).orThrow
        result <- LogStreamIndex.directory(
          directory, filenamePrefix, logLevel, watchGrowth = watchGrowth)
      yield result

  def files(
    files: java.lang.Iterable[Path],
    logLevel: LogLevel,
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.ioRuntime
    given ZoneId = zoneId
    resource_(logLevel):
      for
        given LogFileIndexConf = LogFileIndexConf.fromConfig(ctx.config).orThrow
        result <- LogStreamIndex.files(files.asScala, logLevel)
      yield result

  private def resource_(logLevel: LogLevel)(to: => ResourceIO[LogStreamIndex])
    (using IORuntime)
  : JResource[JLogDirectoryIndex] =
    JResource:
      to.map:
        JLogDirectoryIndex(_)
