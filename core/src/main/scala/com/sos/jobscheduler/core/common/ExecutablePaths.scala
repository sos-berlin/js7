package js7.core.common

import js7.common.scalautil.FileUtils.syntax._
import js7.data.job.ExecutablePath
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object ExecutablePaths
{
  implicit final class RichExecutablePath(private val underlying: ExecutablePath) extends AnyVal
  {
      def toFile(directory: Path): Path =
        directory / underlying.path.stripPrefix("/")
  }
}
