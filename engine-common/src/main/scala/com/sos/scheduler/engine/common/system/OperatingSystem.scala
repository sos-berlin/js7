package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import java.io.File
import scala.collection.immutable
import scala.util.control.NonFatal

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
  private val logger = Logger(getClass)

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

  private case class KernelVersion(kernelName: String, version: immutable.Seq[Int]) {
    def >=(o: KernelVersion) = kernelName == o.kernelName && (version compareElementWise  o.version) >= 0
  }

  private object KernelVersion {
    val Unknown = KernelVersion("UNKNOWN-KERNEL", Nil)

    private lazy val Singleton = ignoreError { KernelVersion(sys.props("os.name"), parseVersion(sys.props("os.version"))) } sideEffect { o ⇒ logger.info(s"$o") }

    def apply(): KernelVersion = Singleton

    private def parseVersion(string: String) = (string split "[.-]" take 3 map { _.toInt }).toList

    private def ignoreError(body: ⇒ KernelVersion): KernelVersion =
      try body
      catch {
        case NonFatal(t) ⇒
          logger.warn(s"Ignored: $t", t)
          Unknown
      }
  }
}

