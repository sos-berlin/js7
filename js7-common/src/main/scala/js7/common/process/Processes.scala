package js7.common.process

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import js7.base.data.ByteArray
import js7.base.generic.GenericLong
import js7.base.time.ScalaTime._
import js7.base.utils.IOUtils.copyStream
import js7.base.utils.ScalaUtils.syntax._
import js7.common.process.OperatingSystemSpecific.OS
import js7.common.process.Processes.RobustlyStartProcess.TextFileBusyIOException
import js7.common.scalautil.IOExecutor.ioFuture
import js7.common.scalautil.{IOExecutor, Logger}
import js7.data.job.ReturnCode
import js7.data.system.StdoutOrStderr
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Await
import scala.concurrent.duration._

object Processes
{
  private val logger = Logger(getClass)

  def processToString(process: Process): String = processToString(process, processToPidOption(process))

  def processToString(process: Process, pid: Option[Pid]) = pid.map(_.toString) getOrElse process.toString

  def processToPidOption(process: Process): Option[Pid] = ProcessPidRetriever.processToPid(process)

  final case class Pid(number: Long) extends GenericLong {
    def string = number.toString
  }

  object Pid extends GenericLong.Companion[Pid]

  /**
   * Builds an argument list for [[ProcessBuilder]].
   */
  def toShellCommandArguments(file: Path, arguments: Seq[String] = Nil): Seq[String] = Vector(file.toString) ++ arguments


  // Shortcuts for operating system specific methods

  /**
    * Including dot.
    * For example ".sh" or ".cmd".
    */
  val ShellFileExtension = OS.shellFileExtension

  val ShellFileAttributes: Seq[FileAttribute[java.util.Set[_]]] = OS.shellFileAttributes

  def newTemporaryShellFile(name: String): Path = OS.newTemporaryShellFile(name)

  def newLogFile(directory: Path, name: String, outerr: StdoutOrStderr): Path = OS.newLogFile(directory, name, outerr)

  def directShellCommandArguments(argument: String): Seq[String] = OS.directShellCommandArguments(argument)

  @TestOnly
  def runProcess(commandLine: String): String = {
    import scala.sys.process._
    logger.debug(commandLine)

    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val exitCode = commandLine.!(new ProcessLogger {
      def out(line: => String): Unit = {
        stdout ++= line
        stdout += '\n'
      }

      def err(line: => String): Unit = {
        stdout ++= line
        stdout += '\n'
      }

      def buffer[T](f: => T) = f
    })
    if (exitCode != 0)
      throw new ProcessException(commandLine, ReturnCode(exitCode), ByteArray(stdout.toString), ByteArray(stderr.toString))
    stdout.toString
  }

  private def runProcess(commandLine: String)(implicit iox: IOExecutor): ByteArray = {
    // —— Does not interpret commandLine as expected ——
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream

    lazy val stdout = ByteArray.unsafeWrap(out.toByteArray)
    lazy val stderr = ByteArray.unsafeWrap(err.toByteArray)

    val processBuilder = new ProcessBuilder(commandLine)
    val process = processBuilder.start()
    process.getInputStream.close()
    val stdoutClosed = ioFuture {
      copyStream(process.getInputStream, out)
    }
    val stderrClosed = ioFuture {
      copyStream(process.getErrorStream, err)
    }
    Await.result(stdoutClosed, Duration.Inf)
    Await.result(stderrClosed, Duration.Inf)
    val returnCode = process.waitFor()
    if (returnCode != 0)
      throw new ProcessException(commandLine, ReturnCode(returnCode), stdout, stderr)
    stdout
  }

  final class ProcessException(commandLine: String, returnCode: ReturnCode, stdout: ByteArray, stderr: ByteArray) extends RuntimeException
  {
    override def getMessage =
      s"""Command failed with exit code ${returnCode.number}
         |$commandLine
         |""".stripMargin +
        Seq(stderr.utf8String, stdout.utf8String).mkString("\n")
  }


  implicit final class RobustlyStartProcess(private val delegate: ProcessBuilder) extends AnyVal {
    /**
      * Like ProcessBuilder.start, but retries after IOException("error=26, Text file busy").
      *
      * @see https://change.sos-berlin.com/browse/JS-1581
      * @see https://bugs.openjdk.java.net/browse/JDK-8068370
      */
    def startRobustly(durations: Iterator[FiniteDuration] = RobustlyStartProcess.DefaultDurations.iterator)
    : Process =
      try delegate.start()
      catch {
        case TextFileBusyIOException(e) if durations.hasNext =>
          logger.warn(s"Retrying process start after error: $e")
          // TODO make startRobustly asynchronoous
          sleep(durations.next())
          startRobustly(durations)
      }
  }

  private[process] object RobustlyStartProcess {
    private val DefaultDurations = List(10.ms, 50.ms, 500.ms, 1440.ms) ensuring { o => o.map(_.toMillis).sum.ms == 2.s }

    object TextFileBusyIOException {
      private def matchesError26(o: String) = """.*\berror=26\b.*""".r.pattern.matcher(o)
      def unapply(e: IOException): Option[IOException] =
        matchesError26(Option(e.getMessage) getOrElse "").matches option e
    }
  }
}
