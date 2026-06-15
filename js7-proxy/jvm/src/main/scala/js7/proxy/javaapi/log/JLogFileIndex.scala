package js7.proxy.javaapi.log

import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.OptionalLong
import java.util.concurrent.CompletableFuture
import js7.base.log.AnsiEscapeCodes.removeHighlights
import js7.base.log.reader.LogFileIndex
import js7.base.log.reader.LogFileUtils.applyLogSelection
import js7.data_for_java.reactor.ReactorConverters.asFlux
import js7.proxy.javaapi.JProxyContext
import reactor.core.publisher.Flux
import scala.jdk.OptionConverters.*
import scala.jdk.OptionShape.*

final class JLogFileIndex(logFileIndex: LogFileIndex)(using IORuntime):

  def lineFlux(begin: Instant, logSelection: JLogSelection): Flux[String] =
    logFileIndex.streamPosAndLine(begin = begin, logSelection.asScala.forReader)
      .through:
        applyLogSelection(logSelection.asScala)(using logFileIndex.zoneId)
      .map: posAndLine =>
        removeHighlights(posAndLine.lineAsString)
    .asFlux

  def instantToFilePosition(instant: Instant, logSelection: JLogSelection)
  : CompletableFuture[OptionalLong] =
    logFileIndex.instantToFilePosition(instant, logSelection.asScala)
      .map(_.toJavaPrimitive)
      .unsafeToCompletableFuture()


object JLogFileIndex:

  def build(file: Path, zoneId: ZoneId, context: JProxyContext): CompletableFuture[JLogFileIndex] =
    build(file, label = file.getFileName.toString, zoneId, context)

  def build(file: Path, label: String, zoneId: ZoneId, context: JProxyContext)
  : CompletableFuture[JLogFileIndex] =
    import context.ioRuntime
    LogFileIndex.fromFile(file, label)(using zoneId)
      .map(JLogFileIndex(_))
      .unsafeToCompletableFuture()
