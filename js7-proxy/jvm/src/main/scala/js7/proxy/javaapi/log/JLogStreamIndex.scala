package js7.proxy.javaapi.log

import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.concurrent.CompletableFuture
import java.util.{Optional, List as JList}
import js7.base.log.reader.recompressors.LogFileIndexConf
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogLineKey, LogStreamIndex}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data_for_java.reactor.ReactorConverters.asFluxChunks
import js7.proxy.javaapi.{JProxyContext, JResource}
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final class JLogStreamIndex private[log](asScala: LogStreamIndex)(using IORuntime)
extends JLogIndex:

  def byteLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[Array[Byte]]] =
    asScala.byteLineStream(begin, logSelection.asScala)
      .map(_.toArray)
      .asFluxChunks

  def byteLogLineFlux(key: LogLineKey, logSelection: JLogSelection)
  : Flux[JList[Array[Byte]]] =
    asScala.byteLineStream(key, logSelection.asScala)
      .map(_.toArray)
      .asFluxChunks

  def stringLogLineFlux(begin: Instant, logSelection: JLogSelection)
  : Flux[JList[String]] =
    asScala.stringLineStream(begin, logSelection.asScala)
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
    asScala.keyedByteLogLineStream(begin, logSelection.asScala)
      .asFluxChunks

  private def keyedLogLineFlux_(begin: Instant | LogLineKey, logSelection: JLogSelection)
  : Flux[JList[KeyedLogLine]] =
    asScala.keyedByteLogLineStream(begin, logSelection.asScala)
      .map(_.toKeyedLogLine)
      .asFluxChunks

  def instantToLogLineKey(instant: Instant, logSelection: JLogSelection)
  : CompletableFuture[Optional[LogLineKey]] =
    asScala.instantToLogLineKey(instant, logSelection.asScala)
      .map(_.toJava)
      .unsafeToCompletableFuture()


object JLogStreamIndex:

  /** Make a JLogStreamIndex for specific files containing a continuous stream of log files.
    * @param files
    * @param zoneId ZoneId for timestamps without timezone
    * @param label a short label for logging
    * @param ctx
    */
  def files(
    files: java.lang.Iterable[Path],
    zoneId: ZoneId,
    label: String,
    ctx: JProxyContext)
  : JResource[JLogStreamIndex] =
    import ctx.ioRuntime
    given ZoneId = zoneId
    JResource:
      for
        given LogFileIndexConf = LogFileIndexConf.fromConfig(ctx.config).orThrow
        result <- LogStreamIndex.files(files.asScala, label = label)
      yield
        JLogStreamIndex(result)
