package js7.base.io.process

import cats.effect.{IO, Resource, Sync}
import java.io.{ByteArrayOutputStream, IOException}
import java.nio.file.attribute.FileAttribute
import java.nio.file.{Files, Path}
import js7.base.data.ByteArray
import js7.base.io.process.OperatingSystemSpecific.OS
import js7.base.io.process.Processes.RobustlyStartProcess.TextFileBusyIOException
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.thread.IOExecutor
import js7.base.thread.IOExecutor.ioFuture
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Await
import scala.concurrent.duration.*

object Processes:

  private val logger = Logger[this.type]

  def processToString(process: Process): String =
    processToString(process, processToPidOption(process))

  def processToString(process: Process, pid: Option[Pid]): String =
    pid.map(_.toString) getOrElse process.toString

  def processToPidOption(process: Process): Option[Pid] =
    ProcessPidRetriever.processToPid(process)


  /**
   * Builds an argument list for [[ProcessBuilder]].
   */
  def toShellCommandArguments(file: Path, arguments: Seq[String] = Nil): Seq[String] =
    Vector(file.toString) ++ arguments


  // Shortcuts for operating system specific methods

  /**
    * Including dot.
    * For example ".sh" or ".cmd".
    */
  val ShellFileExtension: String =
    OS.shellFileExtension

  val ShellFileAttributes: Seq[FileAttribute[java.util.Set[?]]] =
    OS.shellFileAttributes

  def newTemporaryShellFile(name: String): Path =
    OS.newTemporaryShellFile(name)

  def temporaryShellFileResource[F[_]](name: String)(using F: Sync[F]): Resource[F, Path] =
    Resource.make(
      acquire = F.interruptible:
        OS.newTemporaryShellFile(name))(
      release = file => F.interruptible:
        Files.delete(file))

  def newLogFile(directory: Path, name: String, outerr: StdoutOrStderr): Path =
    OS.newLogFile(directory, name, outerr)

  def directShellCommandArguments(argument: String): Seq[String] =
    OS.directShellCommandArguments(argument)

  @TestOnly
  def runProcess(commandLine: String): String =
    import scala.sys.process.*
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
    if exitCode != 0 then
      throw new ProcessException(commandLine, ReturnCode(exitCode), ByteArray(stdout.toString),
        ByteArray(stderr.toString))
    stdout.toString

  private def runProcess(commandLine: String)(implicit iox: IOExecutor): ByteArray =
    // —— Does not interpret commandLine as expected ——
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream

    lazy val stdout = ByteArray.unsafeWrap(out.toByteArray)
    lazy val stderr = ByteArray.unsafeWrap(err.toByteArray)

    val processBuilder = new ProcessBuilder(commandLine)
    val process = processBuilder.start()
    process.getInputStream.close()
    val stdoutClosed = ioFuture:
      process.getInputStream.transferTo(out)
    val stderrClosed = ioFuture:
      process.getErrorStream.transferTo(err)
    Await.result(stdoutClosed, Duration.Inf)
    Await.result(stderrClosed, Duration.Inf)
    val returnCode = process.waitFor()
    if returnCode != 0 then
      throw new ProcessException(commandLine, ReturnCode(returnCode), stdout, stderr)
    stdout

  final class ProcessException(
    commandLine: String, returnCode: ReturnCode, stdout: ByteArray, stderr: ByteArray)
  extends RuntimeException:
    override def getMessage: String =
      s"""Command failed with exit code ${returnCode.number}
         |$commandLine
         |""".stripMargin +
        Seq(stderr.utf8String, stdout.utf8String).mkString("\n")

  implicit final class RobustlyStartProcess(private val processBuilder: ProcessBuilder) extends AnyVal:
    /**
     * Like ProcessBuilder.start, but retries after IOException("error=26, Text file busy").
     *
     * @see https://change.sos-berlin.com/browse/JS-1581
     * @see https://bugs.openjdk.java.net/browse/JDK-8068370
     */
    def startRobustly(durations: Iterable[FiniteDuration] = RobustlyStartProcess.DefaultDurations)
    : IO[Process] =
      IO.defer:
        val durationsIterator = durations.iterator
        IO.blocking:
          processBuilder.start()
        .onErrorRestartLoop(()):
          case (TextFileBusyIOException(e), _, restart) if durationsIterator.hasNext =>
            logger.warn(s"Retrying process start after error: ${e.toString}")
            restart(()).delayBy(durationsIterator.next())

          case (throwable, _, _) =>
            IO.raiseError(throwable)

  private[process] object RobustlyStartProcess:
    private val DefaultDurations = List(10.ms, 50.ms, 500.ms, 1440.ms)
    assert(DefaultDurations.map(_.toMillis).sum.ms == 2.s)

    object TextFileBusyIOException:
      private def matchesError26(o: String) = """.*\berror=26\b.*""".r.pattern.matcher(o)
      def unapply(e: IOException): Option[IOException] =
        matchesError26(Option(e.getMessage) getOrElse "").matches option e
