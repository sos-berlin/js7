package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.io.{File, FileInputStream}
import java.nio.file.Files.newDirectoryStream
import java.nio.file.{Path, Paths}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
trait OperatingSystem {

  def makeExecutableFilename(name: String): String

  def getDynamicLibraryEnvironmentVariableName: String

  def hostname: String = alternativeHostname

  def distributionNameAndVersionOption: Option[String]

  protected def alternativeHostname: String
}

object OperatingSystem {
  private val logger = Logger(getClass)
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

    def distributionNameAndVersionOption = None
  }

  final class Unix private[system] extends OperatingSystem {
    def makeExecutableFilename(name: String): String = name

    def getDynamicLibraryEnvironmentVariableName: String = "LD_LIBRARY_PATH"

    protected def alternativeHostname: String = sys.env.getOrElse("HOSTNAME", "")

    lazy val KernelSupportsNestedShebang = { KernelVersion() >= KernelVersion("Linux", List(2, 6, 28)) }  // Exactly 2.6.27.9 - but what means the fouth number? http://www.in-ulm.de/~mascheck/various/shebang/#interpreter-script

    lazy val distributionNameAndVersionOption: Option[String] = {
      def readFirstLine(file: Path) = autoClosing(new FileInputStream(file)) { in ⇒ io.Source.fromInputStream(in).getLines().next().trim }
      try Some(readFirstLine(Paths.get("/etc/system-release")))
      catch { case NonFatal(_) ⇒
        try Some(readFirstLine(autoClosing(newDirectoryStream(Paths.get("/etc"), "*-release")) { _.iterator().next() }))
        catch { case NonFatal(t) ⇒
          logger.debug(s"distributionNameAndVersion: ignoring $t")
          None
        }
      }
    }
  }

  def concatFileAndPathChain(f: File, pathChain: String): String = {
    val abs = f.getAbsolutePath
    abs +: (pathChain split File.pathSeparator filter { o ⇒ o.nonEmpty && o != abs }) mkString File.pathSeparatorChar.toString
  }
}
