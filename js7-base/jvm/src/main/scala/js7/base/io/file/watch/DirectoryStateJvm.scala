package js7.base.io.file.watch

import cats.effect.IO
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.metering.CallMeter

object DirectoryStateJvm:

  private val meterReadDirectory = CallMeter("DirectoryStateJvm.readDirectory")

  /** @return IO[DirectoryState], maybe failed with IOException */
  def readDirectory(directory: Path, matches: Path => Boolean = _ => true): IO[DirectoryState] =
    meterReadDirectory:
      directory.directoryStream[IO]
        .map(_.getFileName)
        .filter(matches)
        .compile.to(Set)
        .map(DirectoryState(_))
