package js7.core.common

import java.nio.file.Path
import js7.common.scalautil.FileUtils.syntax._
import js7.data.job.ExecutablePath

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
