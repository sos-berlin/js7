package js7.base.system

import java.io.{File, FileInputStream}
import java.net.InetAddress
import java.nio.file.Files.newDirectoryStream
import java.nio.file.attribute.PosixFilePermissions.asFileAttribute
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import java.nio.file.{Path, Paths}
import java.util
import js7.base.system.OperatingSystem.{isMac, isSolaris, isWindows}
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import scala.concurrent.duration.*
import scala.io.Source.fromInputStream
import scala.sys.process.*
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
trait ServerOperatingSystem:

  def secretFileAttributes: Seq[FileAttribute[java.util.Set[?]]]

  def makeExecutableFilename(name: String): String

  def dynamicLibraryEnvironmentVariableName: String

  final lazy val hostname: String =
    sys.env.getOrElse(hostnameEnvName, InetAddress.getLocalHost.getHostName)

  protected def hostnameEnvName: String

  def distributionNameAndVersionOption: Option[String]

  def cpuModel: Option[String]

  def sleepingShellScript(duration: FiniteDuration): String


object ServerOperatingSystem:
  lazy val unix = new Unix
  lazy val windows = new Windows
  val LineEnd: String = if isWindows then "\r\n" else "\n"
  val operatingSystem: ServerOperatingSystem = if isWindows then windows else unix
  val javaLibraryPathPropertyName = "java.library.path"

  lazy val KernelSupportsNestedShebang: Boolean =
    KernelVersion() >= KernelVersion("Linux", List(2, 6, 28))  // Exactly 2.6.27.9 - but what means the fourth number? http://www.in-ulm.de/~mascheck/various/shebang/#interpreter-script

  def makeModuleFilename(path: String): String =
    val file = new File(path)
    new File(file.getParent, System.mapLibraryName(file.getName)).toString

  def makeExecutableFilename(name: String): String = operatingSystem.makeExecutableFilename(name)

  def getDynamicLibraryEnvironmentVariableName: String = operatingSystem.dynamicLibraryEnvironmentVariableName

  final class Windows private[system] extends ServerOperatingSystem:
    val secretFileAttributes: Seq[FileAttribute[util.Set[_]]] = Nil  // TODO File must not be accessible for other Windows users

    def makeExecutableFilename(name: String): String = name + ".exe"

    val dynamicLibraryEnvironmentVariableName = "Path"

    protected val hostnameEnvName = "COMPUTERNAME"

    val distributionNameAndVersionOption: Option[String] = None

    lazy val cpuModel: Option[String] = sys.env.get("PROCESSOR_IDENTIFIER")

    def sleepingShellScript(duration: FiniteDuration) =
      s"@ping -n ${(duration + 999.ms).toSeconds + 1} 127.0.0.1 >nul"

  final class Unix private[system] extends ServerOperatingSystem:
    val secretFileAttributes: Seq[FileAttribute[util.Set[_]]] =
      Seq(asFileAttribute(PosixFilePermissions fromString "rw-------"))
        .asInstanceOf[Seq[FileAttribute[java.util.Set[?]]]]

    def makeExecutableFilename(name: String): String = name

    val dynamicLibraryEnvironmentVariableName = "LD_LIBRARY_PATH"

    protected val hostnameEnvName = "HOSTNAME"

    lazy val distributionNameAndVersionOption: Option[String] =
      def readFirstLine(file: Path): String =
        autoClosing(new FileInputStream(file.toFile)) { in =>
          fromInputStream(in).getLines().next().trim
        }

      def readFileOsRelease() =
        val prettyNamePrefix = "PRETTY_NAME="
        val file = "/etc/os-release"
        autoClosing(new FileInputStream(file)) { in =>    // https://man7.org/linux/man-pages/man5/os-release.5.html
          fromInputStream(in).getLines() collectFirst:
            case line if line startsWith prettyNamePrefix =>
              line.stripPrefix(prettyNamePrefix).stripPrefix("\"").stripPrefix("'").stripSuffix("\"").stripSuffix("'").trim
        }
        .getOrElse(sys.error(s"Key PRETTY_NAME not found in files $file"))

      def readFileAnyRelease() =
        val anyFile = autoClosing(newDirectoryStream(Paths.get("/etc"), "*-release")) { stream =>
          val iterator = stream.iterator
          if !iterator.hasNext then sys.error("No file matches /etc/*-release")
          iterator.next
        }
        readFirstLine(anyFile)

      if isMac then
        Try {
          val RegEx = "([a-zA-Z]+):\t+(.+)$".r
          val lines = ("/usr/bin/sw_vers").!!.trim.split('\n').toVector
          def read(key: String): Option[String] =
            lines.collectFirst:
              case RegEx(`key`, value) => value
          Vector(
            read("ProductName"),
            read("ProductVersion"),
            read("BuildVersion").map("build " + _)
          ).flatten.mkString(" ").??
        }.toOption.flatten
      else if isSolaris then
        None
      else
        Try { readFirstLine(Paths.get("/etc/system-release")) }  // Best result under CentOS 7.2 (more version details than in /etc/os-release)
          .recover { _ => readFileOsRelease() }   // New standard ?
          .recover { _ => readFileAnyRelease() }  // Vendor-specific
          .recover { _ => readFirstLine(Paths.get("/etc/release")) } // Solaris ?
          .toOption

    lazy val cpuModel: Option[String] =
      val fromOS =
        if isMac then
          Try(("/usr/sbin/sysctl -n machdep.cpu.brand_string").!!.trim).toOption
        else if isSolaris then
          None
        else
          Try {
            val CpuModelRegex = """model name[ \t]*:[ \t]*(.+)""".r
            autoClosing(new FileInputStream("/proc/cpuinfo")) { in =>
              fromInputStream(in).getLines().collectFirst:  // We return the model of the first core
                case CpuModelRegex(model) => model.trim.replaceAll("""[ \t\n]+""", " ")
            }
          } .toOption.flatten
      fromOS orElse sys.props.get("os.arch")

    def sleepingShellScript(duration: FiniteDuration) =
      s"sleep ${(duration + 999.ms).toSeconds}\n"

  def concatFileAndPathChain(f: File, pathChain: String): String =
    val abs = f.getAbsolutePath
    abs +: (pathChain split File.pathSeparator).filter(o => o.nonEmpty && o != abs) mkString File.pathSeparatorChar.toString
