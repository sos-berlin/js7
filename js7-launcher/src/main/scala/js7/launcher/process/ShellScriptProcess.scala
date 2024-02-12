package js7.launcher.process

import fs2.Stream
import cats.effect.{Deferred, FiberIO, IO}
import fs2.concurrent.Channel
import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
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

  def watchProcess: IO[ReturnCode]

  stdin.close() // Process gets an empty stdin

  private val sigkilled = Deferred.unsafe[IO, Unit]

  protected final val whenSigkilled: IO[Unit] =
    sigkilled.get

  override protected def onSigkill =
    // We do not super.onSigkill(), because Process.destroyForcibly closes stdout and stderr
    // leading to blocked child processes trying to write to stdout (as observed by a customer).
    sigkilled.complete(()).void


object ShellScriptProcess:
  private val logger = Logger[this.type]
  private val stdoutAndStderrDetachDelay = 1.s  // Grace period between kill and destroyForcibly

  def startPipedShellScript(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    onTerminated: IO[Unit] = IO.unit)
    (implicit iox: IOExecutor)
  : IO[Checked[ShellScriptProcess]] =
    IO.defer:
      val commandArgs = toShellCommandArguments(
        commandLine.file,
        commandLine.arguments.tail ++ conf.idArgumentOption /*TODO Should not be an argument*/)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf)
        .map(_.map: process =>
          new ShellScriptProcess(conf, process):
            val watchProcess = watchProcess2.unsafeMemoize
            override val terminated = watchProcess

            private def copyToStream(
              outErr: StdoutOrStderr,
              in: InputStream,
              channel: Channel[IO, Either[Throwable, String]])
            : IO[Unit] =
              copyInputStreamToStringChannel(outErr, in, channel, conf.encoding, stdObservers.charBufferSize)
                .recover {
                  case t: IOException if isKilling /*Happens under Windows*/ => logger.warn(
                    s"While killing the process, $outErr become unreadable: ${t.toStringWithCauses}",
                    t)
                }

            def await(outerr: StdoutOrStderr, fiber: FiberIO[Unit]): IO[Unit] =
              fiber.joinStd
                .handleError(t => logger.warn(outerr.toString + ": " + t.toStringWithCauses))

            private def watchProcess2: IO[ReturnCode] =
              for
                outFiber <- copyToStream(Stdout, process.stdout, stdObservers.outChannel).start
                errFiber <- copyToStream(Stderr, process.stderr, stdObservers.errChannel).start
                _ <-
                  IO.race(
                    // Sometimes, the process is not really killed.
                    // Then we destroyForcibly. Because this closes stdout and stderr, we
                    // wait a short while to allow the last outstanding data to handled by
                    // copyToStream. See also RichProcess superclass.
                    whenSigkilled.delayBy(stdoutAndStderrDetachDelay).map: _ =>
                      IO.whenA(process.isAlive):
                        IO(logger.debug(s"destroyForcibly $process")) *>
                        IO.blocking(process.destroyForcibly()),
                    IO.both(
                      await(Stdout, outFiber),
                      await(Stderr, errFiber)))
                _ <- stdObservers.closeChannels
                returnCode <- super.terminated
                _ <- onTerminated
              yield returnCode)

            //private def watchProcess2: IO[ReturnCode] =
            //  IO
            //    .both(
            //      copyToStream(Stdout, process.stdout),
            //      copyToStream(Stderr, process.stderr))
            //    .background.use: outErrStreaming =>
            //      for
            //        _ <-
            //          IO.race(
            //            whenSigkilled
            //              // Sometimes, the process is not really killed.
            //              // Then we destroyForcibly. Because this closes stdout and stderr, we
            //              // wait a short while to allow the last outstanding data to handled by
            //              // copyToStream. See also RichProcess superclass.
            //              .andWait(stdoutAndStderrDetachDelay)
            //              .flatTap: _ =>
            //                IO.whenA(process.isAlive)(IO.defer:
            //                    logger.debug(s"destroyForcibly $process")
            //                    IO.blocking:
            //                      process.destroyForcibly())
            //                  .*>(stdObservers.closeChannels),
            //            outErrStreaming)
            //        //_ <-
            //        //  whenSigkilled
            //        //    // Sometimes, the process is not really killed.
            //        //    // Then we destroyForcibly. Because this closes stdout and stderr, we
            //        //    // wait a short while to allow the last outstanding data to handled by
            //        //    // copyToStream. See also RichProcess superclass.
            //        //    .andWait(stdoutAndStderrDetachDelay)
            //        //    .flatTap: _ =>
            //        //      IO.whenA(process.isAlive)(IO.defer:
            //        //        logger.debug(s"destroyForcibly $process")
            //        //        IO.blocking:
            //        //          process.destroyForcibly())
            //        //      .*>(stdObservers.closeChannels)
            //        //  .background.surround:
            //        //    outErrStreaming
            //        _ <- stdObservers.closeChannels
            //        returnCode <- super.terminated
            //        _ <- onTerminated
            //      yield returnCode)

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
