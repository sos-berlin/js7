package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import java.io.{File, FileInputStream}
import java.nio.file.Files.newDirectoryStream
import java.nio.file.{Path, Paths}
import scala.io
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
trait OperatingSystem {

  def makeExecutableFilename(name: String): String

  def getDynamicLibraryEnvironmentVariableName: String

  def hostname: String = alternativeHostname

  def distributionNameAndVersionOption: Option[String]

  def cpuModel: Option[String]

  protected def alternativeHostname: String
}

object OperatingSystem {
  val name: String = sys.props("os.name")
  val cpuArchitecture = CpuArchitecture.cpuArchitecture
  val isWindows = name startsWith "Windows"
  val isUnix = !isWindows
  val isSolaris = name startsWith "SunOS"
  lazy val unix = new Unix
  lazy val windows = new Windows
  val operatingSystem: OperatingSystem = if (isWindows) windows else unix
  val javaLibraryPathPropertyName = "java.library.path"
  lazy val KernelSupportsNestedShebang = KernelVersion() >= KernelVersion("Linux", List(2, 6, 28))  // Exactly 2.6.27.9 - but what means the fourth number? http://www.in-ulm.de/~mascheck/various/shebang/#interpreter-script

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

    def cpuModel = sys.env.get("PROCESSOR_IDENTIFIER")
  }

  final class Unix private[system] extends OperatingSystem {
    def makeExecutableFilename(name: String): String = name

    def getDynamicLibraryEnvironmentVariableName: String = "LD_LIBRARY_PATH"

    protected def alternativeHostname: String = sys.env.getOrElse("HOSTNAME", "")

    lazy val distributionNameAndVersionOption: Option[String] = {
      def readFirstLine(file: Path): String =
        autoClosing(new FileInputStream(file)) { in ⇒
          io.Source.fromInputStream(in).getLines.next().trim
        }

      def readFileOsRelease() = {
        val prettyNamePrefix = "PRETTY_NAME="
        val file = "/etc/os-release"
        autoClosing(new FileInputStream(file)) { in ⇒    // http://man7.org/linux/man-pages/man5/os-release.5.html
          io.Source.fromInputStream(in).getLines collectFirst {
            case line if line startsWith prettyNamePrefix ⇒
              line.stripPrefix(prettyNamePrefix).stripPrefix("\"").stripPrefix("'").stripSuffix("\"").stripSuffix("'").trim
          }
        }
        .getOrElse { sys.error(s"Key PRETTY_NAME not found in file $file")}
      }

      def readFileAnyRelease() = {
        val anyFile = autoClosing(newDirectoryStream(Paths.get("/etc"), "*-release")) { stream ⇒
          val iterator = stream.iterator
          if (!iterator.hasNext) throw sys.error("No file matches /etc/*-release")
          iterator.next
        }
        readFirstLine(anyFile)
      }

      Try { readFirstLine(Paths.get("/etc/system-release")) }  // Best result under CentOS 7.2 (more version details than in /etc/os-release)
        .recover { case _ ⇒ readFileOsRelease() }   // New standard ?
        .recover { case _ ⇒ readFileAnyRelease() }  // Vendor-specific
        .recover { case _ ⇒ readFirstLine(Paths.get("/etc/release")) } // Solaris ?
        .toOption
    }

    def cpuModel =
      Try {
        val CpuModelRegex = """model name[ \t]*:[ \t]*(.+)""".r
        autoClosing(new FileInputStream("/proc/cpuinfo")) { in ⇒
          io.Source.fromInputStream(in).getLines collectFirst {  // Assuming all cores are of same model
            case CpuModelRegex(model) ⇒ model.trim
          }
        }
      } .toOption.flatten
  }

  def concatFileAndPathChain(f: File, pathChain: String): String = {
    val abs = f.getAbsolutePath
    abs +: (pathChain split File.pathSeparator filter { o ⇒ o.nonEmpty && o != abs }) mkString File.pathSeparatorChar.toString
  }
}
