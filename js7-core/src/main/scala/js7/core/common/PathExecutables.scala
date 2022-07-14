package js7.core.common

import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.*
import js7.data.job.PathExecutable

/**
  * @author Joacim Zschimmer
  */
private object PathExecutables
{
  implicit final class RichPathExecutable(private val underlying: PathExecutable) extends AnyVal
  {
      def toFile(directory: Path): Path =
        directory / underlying.path.stripPrefix("/")
  }
}
