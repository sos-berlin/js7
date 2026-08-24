package js7.proxy.javaapi.log

import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import java.nio.file.Path
import java.time.ZoneId
import java.util.Set as JSet
import java.util.concurrent.CompletableFuture
import js7.base.log.LogLevel
import js7.base.log.reader.LogDirectoryIndex
import js7.proxy.javaapi.{JProxyContext, JResource}
import scala.jdk.CollectionConverters.*

final class JLogDirectoryIndex private(asScala: LogDirectoryIndex)(using IORuntime):

  def logIndex(logFilePrefix: String, logLevel: LogLevel): CompletableFuture[JLocalLogIndex] =
    asScala.logIndex(logFilePrefix, logLevel)
      .map(JLocalLogIndex(_))
      .unsafeToCompletableFuture()


object JLogDirectoryIndex:

  /** Provides the log files of a living log directory.
    *
    * @param directory      Watched directory containing the log files
    * @param logFilePrefixes Log file name prefixes that should be watched,
    *                        for example "joc" (as in joc.log filename)
    * @param zoneId         normally ZoneId.systemDefault
    * @param ctx            The runtime
    */
  def directory(
    directory: Path,
    logFilePrefixes: JSet[String],
    zoneId: ZoneId,
    ctx: JProxyContext)
  : JResource[JLogDirectoryIndex] =
    import ctx.ioRuntime
    given ZoneId = zoneId
    JResource:
      locally:
        for
          given Config = ctx.config
          result <- LogDirectoryIndex.resource(directory, logFilePrefixes.asScala.toSet)
        yield result
      .map:
        JLogDirectoryIndex(_)
