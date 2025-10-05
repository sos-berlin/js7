package js7.base.io.process

import cats.effect.{IO, Outcome, Resource, Sync}
import fs2.Stream
import java.nio.file.attribute.FileAttribute
import java.nio.file.{Files, Path}
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.io.ReaderStreams.inputStreamToByteStream
import js7.base.io.process.OperatingSystemSpecific.OS
import js7.base.io.process.StartRobustly.startRobustly
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
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

  def directShellCommandArguments(argument: String): Seq[String] =
    OS.directShellCommandArguments(argument)

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

  private def waitForProcessTermination(process: Process): IO[ReturnCode] =
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
