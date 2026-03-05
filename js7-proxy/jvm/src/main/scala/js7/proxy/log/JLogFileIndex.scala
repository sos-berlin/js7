package js7.proxy.log

import cats.effect.unsafe.IORuntime
import java.nio.file.Path
import java.time.{Instant, ZoneId}
import java.util.concurrent.CompletableFuture
import java.util.{Optional, OptionalLong}
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.AnsiEscapeCodes.removeHighlights
import js7.base.log.reader.LogFileIndex
import js7.data_for_java.reactor.ReactorConverters.asFlux
import js7.proxy.javaapi.JProxyContext
import reactor.core.publisher.Flux
import scala.jdk.OptionConverters.*
import scala.jdk.OptionShape.*

final class JLogFileIndex(logFileIndex: LogFileIndex)(using IORuntime):

  def lineFlux(begin: Instant, end: Optional[Instant]): Flux[String] =
    fs2.Stream.resource:
      logFileIndex.logFileReader()
    .flatMap:
      logFileIndex.streamPosAndLine(_, begin = begin, end = end.toScala)
        .map: (_, byteLine) =>
          removeHighlights(byteLine.utf8String)
    .asFlux

  def instantToFilePosition(instant: Instant): CompletableFuture[OptionalLong] =
    logFileIndex.logFileReader().use:
      logFileIndex.instantToFilePosition(_, instant)
    .map(_.toJavaPrimitive)
    .unsafeToCompletableFuture()


object JLogFileIndex:

  def build(file: Path, zoneId: ZoneId, context: JProxyContext): CompletableFuture[JLogFileIndex] =
    build(file, label = file.getFileName.toString, zoneId, context)

  def build(file: Path, label: String, zoneId: ZoneId, context: JProxyContext)
  : CompletableFuture[JLogFileIndex] =
    import context.given_IORuntime
    LogFileIndex.build(file, label)(using zoneId)
      .map(JLogFileIndex(_))
      .unsafeToCompletableFuture()
