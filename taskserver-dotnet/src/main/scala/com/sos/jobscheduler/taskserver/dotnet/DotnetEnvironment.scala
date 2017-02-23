package com.sos.jobscheduler.taskserver.dotnet

import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.taskserver.dotnet.dlls.DotnetDlls
import java.nio.file.Files._
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
final class DotnetEnvironment(baseTemporaryDirectory: Path) extends HasCloser {

  val directory = createTempDirectory(baseTemporaryDirectory, "dotnet")
  private val files = DotnetDlls.provideDlls(directory)

  onClose {
    files foreach deleteIfExists
    deleteIfExists(directory)
  }

  override def toString = s"DotnetEnvironment $directory"
}
