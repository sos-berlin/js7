package js7.launcher.process

import cats.effect.{Deferred, FiberIO, IO}
import fs2.concurrent.Channel
import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.CommandLine
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import js7.launcher.process.CopyInputStreamToStringChannel.copyInputStreamToStringChannel
import scala.jdk.CollectionConverters.*

abstract class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit iox: IOExecutor)
extends RichProcess(processConfiguration, process):

  stdin.close() // Process gets an empty stdin

  private val sigkilled = Deferred.unsafe[IO, Unit]

  protected final val whenSigkilled: IO[Unit] =
    sigkilled.get

  def watchProcess: IO[ReturnCode]

  override protected def onSigkill =
    // We do not super.onSigkill(), because Process.destroyForcibly closes stdout and stderr
    // leading to blocked child processes trying to write to stdout (as observed by a customer).
    sigkilled.complete(()).void


object ShellScriptProcess:
  private val logger = Logger[this.type]
  private val stdoutAndStderrDetachDelay = 1.s  // TODO

  def startPipedShellScript(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    whenTerminated: IO[Unit] = IO.unit)
    (implicit iox: IOExecutor)
  : IO[Checked[ShellScriptProcess]] =
    IO.defer:
      val commandArgs = toShellCommandArguments(
        commandLine.file,
        commandLine.arguments.tail ++ conf.idArgumentOption /*TODO Should not be an argument*/)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf)
        .map(_.map { process =>
          new ShellScriptProcess(conf, process) {
            import conf.encoding
            import stdObservers.{charBufferSize, errChannel, outChannel}

            private def copyToStream(
              outErr: StdoutOrStderr,
              in: InputStream,
              channel: Channel[IO, Either[Throwable, String]])
            : IO[Unit] =
              copyInputStreamToStringChannel(outErr, in, channel, encoding, charBufferSize)
                .recover {
                  case t: IOException if isKilling /*Happens under Windows*/ => logger.warn(
                    s"While killing the process, $outErr become unreadable: ${t.toStringWithCauses}")
                }

            def await(outerr: StdoutOrStderr, fiber: FiberIO[Unit]): IO[Unit] =
              fiber.joinStd
                .handleError(t => logger.warn(outerr.toString + ": " + t.toStringWithCauses))

            val watchProcess: IO[ReturnCode] =
              watchProcess2.unsafeMemoize

            override val terminated = watchProcess

            private def watchProcess2: IO[ReturnCode] =
              for
                outFiber <- copyToStream(Stdout, process.stdout, stdObservers.outChannel).start
                errFiber <- copyToStream(Stderr, process.stderr, stdObservers.errChannel).start
                _ <- IO.race(
                  whenSigkilled /*.delayBy(stdoutAndStderrDetachDelay)???*/.map: _ =>
                    if process.isAlive then
                      logger.debug(s"destroyForcibly $process")
                      process.destroyForcibly(),
                  IO.both(
                    await(Stdout, outFiber),
                    await(Stderr, errFiber)))
                returnCode <- super.terminated
                _ <- whenTerminated
              yield returnCode
          }
        })

  private def startProcess(args: Seq[String], conf: ProcessConfiguration)
  : IO[Checked[Js7Process]] =
    conf.windowsLogon match
      case None =>
        val processBuilder = new ProcessBuilder(args.asJava)
        for o <- conf.workingDirectory do processBuilder.directory(o.toFile)

        transferEnv(from = conf.additionalEnvironment, to = processBuilder.environment)

        processBuilder.startRobustly()
          .map(o => Right(JavaProcess(o)))

      case Some(logon) =>
        IO(
          WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              args,
              stdinRedirect = PIPE,
              stdoutRedirect = PIPE,
              stderrRedirect = PIPE,
              additionalEnv = conf.additionalEnvironment),
            Some(logon)))

  private def transferEnv(
    from: Map[String, Option[String]],
    to: java.util.Map[String, String])
  : Unit =
    from
      .collect { case (k, None) => k }
      .foreach(to.remove)

    to.putAll(from
      .collect { case (k, Some(v)) => k -> v }
      .asJava)
