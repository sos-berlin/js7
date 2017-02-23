package com.sos.scheduler.engine.taskserver.dotnet.dlls

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.utils.JavaResource
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
object DotnetDlls {

  val DllName = "com.sos-berlin.engine.engine-job-api-dotnet.dll"
  private val Dir = "com/sos/scheduler/engine/taskserver/dotnet/dlls"
  private val Jni4netDlls = Set(
      sys.props.get("sun.arch.data.model") match {
        case Some("64") ⇒ JavaResource(s"$Dir/jni4net.n.w64.v40-0.8.8.0.dll")
        case Some("32") ⇒ JavaResource(s"$Dir/jni4net.n.w32.v40-0.8.8.0.dll")
        case o ⇒ sys.error(s"Unknown Java property sun.arch.data.model=$o")
      },
      JavaResource(s"$Dir/jni4net.n-0.8.8.0.dll"))
  private[dotnet] val DllsResourcePaths: Set[JavaResource] = Jni4netDlls + JavaResource(s"$Dir/$DllName")
  private val logger = Logger(getClass)

  def provideDlls(directory: Path): Set[Path] =
    for (resource ← DllsResourcePaths) yield {
      val file = directory resolve resource.simpleName
      logger.debug(s"Providing $file")
      resource.copyToFile(file)
      file
    }
}
