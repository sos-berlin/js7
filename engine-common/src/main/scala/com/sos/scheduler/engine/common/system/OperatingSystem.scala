package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.File

/**
 * @author Joacim Zschimmer
 */
trait OperatingSystem {

  def makeExecutableFilename(name: String): String

  def getDynamicLibraryEnvironmentVariableName: String

  def hostname: String = alternativeHostname

  protected def alternativeHostname: String
}

object OperatingSystem {
  val name: String = sys.props("os.name")
  val cpuArchitecture = CpuArchitecture.cpuArchitecture
  val isWindows = name startsWith "Windows"
  val isUnix = !isWindows
  lazy val unix = new Unix
  lazy val windows = new Windows
  val operatingSystem: OperatingSystem = if (isWindows) windows else unix
  val javaLibraryPathPropertyName = "java.library.path"
  lazy val KernelSupportsNestedShebang = { KernelVersion() >= KernelVersion("Linux", List(2, 6, 28)) }  // Exactly 2.6.27.9 - but what means the fouth number? http://www.in-ulm.de/~mascheck/various/shebang/#interpreter-script

  def makeModuleFilename(path: String): String = {
    val file = new File(path)
    new File(file.getParent, System.mapLibraryName(file.getFileName.toString)).toString
  }

  def makeExecutableFilename(name: String): String = operatingSystem.makeExecutableFilename(name)

  def getDynamicLibraryEnvironmentVariableName: String = operatingSystem.getDynamicLibraryEnvironmentVariableName

  final class Windows private[system] extends OperatingSystem {
    def makeExecutableFilename(name: String): String = name + ".exe"

    def getDynamicLibraryEnvironmentVariableName: String = "PATH"

    protected def alternativeHostname: String = sys.env.getOrElse("COMPUTERNAME", "")
  }

  final class Unix private[system] extends OperatingSystem {
    def makeExecutableFilename(name: String): String = name

    def getDynamicLibraryEnvironmentVariableName: String = "LD_LIBRARY_PATH"

    protected def alternativeHostname: String = sys.env.getOrElse("HOSTNAME", "")

    lazy val KernelSupportsNestedShebang = { KernelVersion() >= KernelVersion("Linux", List(2, 6, 28)) }  // Exactly 2.6.27.9 - but what means the fouth number? http://www.in-ulm.de/~mascheck/various/shebang/#interpreter-script
  }

  def concatFileAndPathChain(f: File, pathChain: String): String = {
    val abs = f.getAbsolutePath
    abs +: (pathChain split File.pathSeparator filter { o ⇒ o.nonEmpty && o != abs }) mkString File.pathSeparatorChar.toString
  }

}

