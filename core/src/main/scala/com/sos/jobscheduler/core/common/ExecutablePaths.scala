package com.sos.jobscheduler.core.common

import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.data.job.ExecutablePath
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
