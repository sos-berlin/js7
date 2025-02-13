package js7.base.io.file.watch

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.kernel.Sync.Type.InterruptibleOnce
import java.nio.file.{Files, Path}
import js7.base.catsutils.CatsEffectExtensions.fromAutoCloseableCancelable
import js7.base.metering.CallMeter
import scala.jdk.CollectionConverters.*

object DirectoryStateJvm:

  private val meterReadDirectory = CallMeter("DirectoryStateJvm.readDirectory")

  /** @return IO[DirectoryState], maybe failed with IOException */
  def readDirectory(directory: Path, matches: Path => Boolean = _ => true): IO[DirectoryState] =
    meterReadDirectory:
      Resource.fromAutoCloseableCancelable:
        IO.interruptible:
          Files.list(directory)
      .use: javaStream =>
        fs2.Stream
          .fromIterator[IO](
            javaStream.iterator.asScala,
            chunkSize = 128,
            InterruptibleOnce)
          .filter: path =>
            path.startsWith(directory) // Ignore silently alien paths
          .map:
            directory.relativize
          .filter: path =>
            val s = path.toString
            s != "." && s != ".." && matches(path)
          .map: path =>
            path -> DirectoryState.Entry(path)
          .compile.to(Map)
          .map(DirectoryState(_))
