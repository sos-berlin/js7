package com.sos.jobscheduler.common.system

import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import java.io.{File, FileInputStream}
import java.net.InetAddress
import java.nio.file.Files.newDirectoryStream
import java.nio.file.attribute.PosixFilePermissions.asFileAttribute
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{Path, Paths}
import scala.collection.immutable
import scala.concurrent.duration._
import scala.io.Source.fromInputStream
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
trait OperatingSystem {

  def secretFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]]

  def makeExecutableFilename(name: String): String

  def getDynamicLibraryEnvironmentVariableName: String

  final lazy val hostname: String =
    sys.env.getOrElse(hostnameEnvName, InetAddress.getLocalHost.getHostName)

  protected def hostnameEnvName: String

  def distributionNameAndVersionOption: Option[String]

  def cpuModel: Option[String]

  def sleepingShellScript(duration: FiniteDuration): String
}

object OperatingSystem {
  val name: String = sys.props("os.name")
  val cpuArchitecture = CpuArchitecture.cpuArchitecture
  val isWindows = name startsWith "Windows"
  val isMac = name startsWith "Mac OS"
  val isUnix = !isWindows
  val isSolaris = name startsWith "SunOS"
  lazy val unix = new Unix
  lazy val windows = new Windows
  val LineEnd = if (isWindows) "\r\n" else "\n"
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
    val secretFileAttributes = Nil  // TODO File must not be accessible for other Windows users

    def makeExecutableFilename(name: String): String = name + ".exe"

    def getDynamicLibraryEnvironmentVariableName: String = "PATH"

    protected def hostnameEnvName = "COMPUTERNAME"

    def distributionNameAndVersionOption = None

    def cpuModel = sys.env.get("PROCESSOR_IDENTIFIER")

    def sleepingShellScript(duration: FiniteDuration) =
      s"@ping -n ${(duration + 999.milliseconds).toSeconds + 1} 127.0.0.1 >nul"
  }

  final class Unix private[system] extends OperatingSystem {
    val secretFileAttributes = List(asFileAttribute(PosixFilePermissions fromString "rw-------"))
      .asInstanceOf[immutable.Seq[FileAttribute[java.util.Set[_]]]]

    def makeExecutableFilename(name: String): String = name

    def getDynamicLibraryEnvironmentVariableName: String = "LD_LIBRARY_PATH"

    protected def hostnameEnvName = "HOSTNAME"

    lazy val distributionNameAndVersionOption: Option[String] = {
      def readFirstLine(file: Path): String =
        autoClosing(new FileInputStream(file)) { in =>
          fromInputStream(in).getLines.next().trim
        }

      def readFileOsRelease() = {
        val prettyNamePrefix = "PRETTY_NAME="
        val file = "/etc/os-release"
        autoClosing(new FileInputStream(file)) { in =>    // http://man7.org/linux/man-pages/man5/os-release.5.html
          fromInputStream(in).getLines collectFirst {
            case line if line startsWith prettyNamePrefix =>
              line.stripPrefix(prettyNamePrefix).stripPrefix("\"").stripPrefix("'").stripSuffix("\"").stripSuffix("'").trim
          }
        }
        .getOrElse(sys.error(s"Key PRETTY_NAME not found in files $file"))
      }

      def readFileAnyRelease() = {
        val anyFile = autoClosing(newDirectoryStream(Paths.get("/etc"), "*-release")) { stream =>
          val iterator = stream.iterator
          if (!iterator.hasNext) sys.error("No file matches /etc/*-release")
          iterator.next
        }
        readFirstLine(anyFile)
      }

      if (isMac || isSolaris)
        None
      else
        Try { readFirstLine(Paths.get("/etc/system-release")) }  // Best result under CentOS 7.2 (more version details than in /etc/os-release)
          .recover { case _ => readFileOsRelease() }   // New standard ?
          .recover { case _ => readFileAnyRelease() }  // Vendor-specific
          .recover { case _ => readFirstLine(Paths.get("/etc/release")) } // Solaris ?
          .toOption
    }

    def cpuModel =
      if (isMac || isSolaris)
        None
      else
        Try {
          val CpuModelRegex = """model name[ \t]*:[ \t]*(.+)""".r
          autoClosing(new FileInputStream("/proc/cpuinfo")) { in =>
            fromInputStream(in).getLines collectFirst {  // Assuming all cores are of same model
              case CpuModelRegex(model) => model.trim.replaceAll("""[ \t\n]+""", " ")
            }
          }
        } .toOption.flatten

    def sleepingShellScript(duration: FiniteDuration) =
      s"sleep ${(duration + 999.milliseconds).toSeconds}\n"
  }

  def concatFileAndPathChain(f: File, pathChain: String): String = {
    val abs = f.getAbsolutePath
    abs +: (pathChain split File.pathSeparator filter { o => o.nonEmpty && o != abs }) mkString File.pathSeparatorChar.toString
  }
}
