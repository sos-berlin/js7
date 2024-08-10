package js7.base.io.process

import cats.effect.{IO, Outcome, Resource, Sync}
import fs2.Stream
import java.io.IOException
import java.nio.file.attribute.FileAttribute
import java.nio.file.{Files, Path}
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.io.ReaderStreams.inputStreamToByteStream
import js7.base.io.process.OperatingSystemSpecific.OS
import js7.base.io.process.Processes.RobustlyStartProcess.TextFileBusyIOException
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

object Processes:

  private val logger = Logger[this.type]

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

    val exitCode = commandLine.!(
      new ProcessLogger:
        def out(line: => String): Unit =
          stdout ++= line
          stdout += '\n'

        def err(line: => String): Unit =
          stdout ++= line
          stdout += '\n'

        def buffer[T](f: => T) = f)
    if exitCode != 0 then
      throw new ProcessException(commandLine, ReturnCode(exitCode), ByteArray(stdout.toString),
        ByteArray(stderr.toString))
    stdout.toString

  def runAndLogProcess(args: Seq[String])(logLine: String => IO[Unit]): IO[ReturnCode] =
    def logLines(stream: Stream[IO, Byte]): IO[Unit] =
      stream.chunks.map(_.utf8String).through(fs2.text.lines).evalMap(logLine).compile.drain

    startProcess(args).flatMap: (process, stdout, stderr) =>
      IO.both(
        left = IO.both(logLines(stdout), logLines(stderr))
          .guaranteeCase:
            case Outcome.Succeeded(_) => IO.unit
            case _ =>
              IO.blocking:
                process.destroy()
              .productR:
                waitForProcessTermination(process)
              .startAndForget,
        right = waitForProcessTermination(process))
    .map(_._2)

  def startProcess(path: Path, args: String*): IO[(Process, Stream[IO, Byte], Stream[IO, Byte])] =
    startProcess(path.toString +: args)

  /** @return (Process, stdout as Stream, stderr as Stream) */
  def startProcess(args: Seq[String]): IO[(Process, Stream[IO, Byte], Stream[IO, Byte])] =
    IO.defer:
      logger.trace(s"${args.mkString(" ")}")
      ProcessBuilder(args.asJava)
        .startRobustly()
        .map: process =>
          (process,
            inputStreamToByteStream(process.getInputStream),
            inputStreamToByteStream(process.getErrorStream))

  def waitForProcessTermination(process: Process): IO[ReturnCode] =
    interruptibleVirtualThread:
      logger.traceCallWithResult(s"waitFor ${Pid(process.pid)}"):
        ReturnCode(process.waitFor())


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
