package js7.launcher.process

import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.CommandLine
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import js7.launcher.process.InputStreamToObservable.copyInputStreamToObservable
import monix.eval.{Fiber, Task}
import monix.reactive.Observer
import scala.concurrent.Promise
import scala.jdk.CollectionConverters.*
import scala.util.Try

class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit iox: IOExecutor)
  extends RichProcess(processConfiguration, process)
{
  stdin.close() // Process gets an empty stdin

  private val _sigkilled = Promise[Unit]()

  final val sigkilled: Task[Unit] =
    Task.fromFuture(_sigkilled.future).memoize

  override protected def onSigkill(): Unit = {
    // We do not super.onSigkill(), because Process.destroyForcibly closes stdout and stderr
    // leading to blocked child processes trying to write to stdout (as observed by a customer).
    _sigkilled.trySuccess(())
  }
}

object ShellScriptProcess {
  private val logger = Logger[this.type]
  private val stdoutAndStderrDetachDelay = 1.s  // TODO

  def startPipedShellScript(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    whenTerminated: Task[Unit] = Task.unit)
    (implicit iox: IOExecutor)
  : Task[Checked[ShellScriptProcess]] =
    Task.defer {
      val commandArgs = toShellCommandArguments(
        commandLine.file,
        commandLine.arguments.tail ++ conf.idArgumentOption /*TODO Should not be an argument*/)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf)
        .map(_.map { process =>
          new ShellScriptProcess(conf, process) {
            import conf.encoding
            import stdObservers.{charBufferSize, err, out}

            private def copyToObservable(outErr: StdoutOrStderr, in: InputStream, obs: Observer[String])
            : Task[Unit] =
              copyInputStreamToObservable(in, obs, encoding, charBufferSize)
                .onErrorRecover {
                  case t: IOException if isKilling /*Happens under Windows*/ => logger.warn(
                    s"While killing the process, $outErr become unreadable: ${t.toStringWithCauses}")
                }

            def await(outerr: StdoutOrStderr, fiber: Fiber[Unit]): Task[Unit] =
              fiber.join.onErrorHandle(t => logger.warn(outerr.toString + ": " + t.toStringWithCauses))

            override val terminated =
              (for {
                outFiber <- copyToObservable(Stdout, process.stdout, out).start
                errFiber <- copyToObservable(Stderr, process.stderr, err).start
                _ <- Task.race(
                  sigkilled.delayExecution(stdoutAndStderrDetachDelay).map { _ =>
                    if (false) Try(process.stdout.close()) // FIXME
                    if (false) Try(process.stderr.close()) // FIXME
                    if (process.isAlive) {
                      logger.debug("destroyForcibly")
                      process.destroyForcibly()
                    }
                  },
                  await(Stdout, outFiber) *> await(Stderr, errFiber))
                returnCode <- super.terminated
                _ <- whenTerminated
              } yield returnCode).memoize
          }
        })
    }

  private def startProcess(args: Seq[String], conf: ProcessConfiguration)
  : Task[Checked[Js7Process]] =
    conf.windowsLogon match {
      case None =>
        val processBuilder = new ProcessBuilder(args.asJava)
        for (o <- conf.workingDirectory) processBuilder.directory(o.toFile)

        transferEnv(from = conf.additionalEnvironment, to = processBuilder.environment)

        processBuilder.startRobustly()
          .map(o => Right(JavaProcess(o)))

      case Some(logon) =>
        Task(
          WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              args,
              stdinRedirect = PIPE,
              stdoutRedirect = PIPE,
              stderrRedirect = PIPE,
              additionalEnv = conf.additionalEnvironment),
            Some(logon)))
    }

  private def transferEnv(
    from: Map[String, Option[String]],
    to: java.util.Map[String, String])
  : Unit = {
    from
      .collect { case (k, None) => k }
      .foreach(to.remove)

    to.putAll(from
      .collect { case (k, Some(v)) => k -> v }
      .asJava)
  }
}
