package js7.launcher.process

import java.io.{BufferedOutputStream, OutputStream, OutputStreamWriter}
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes._
import js7.base.io.process.{JavaProcess, Js7Process, ProcessSignal, ReturnCode, StdoutOrStderr}
import js7.base.log.LogLevel.syntax._
import js7.base.log.{LogLevel, Logger}
import js7.base.system.OperatingSystem.{isMac, isWindows}
import js7.base.thread.IOExecutor
import js7.base.thread.IOExecutor.ioTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.launcher.process.RichProcess._
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
class RichProcess protected[process](
  val processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit iox: IOExecutor)
{
  private val runningSince = now
  val pidOption: Option[Pid] = process.pid
  private val logger = Logger.withPrefix[this.type](toString)
  /**
   * UTF-8 encoded stdin.
   */
  final lazy val stdinWriter = new OutputStreamWriter(new BufferedOutputStream(stdin), UTF_8)

  protected def onSigkill(): Unit =
    if (process.isAlive) {
      logger.debug("destroyForcibly")
      process.destroyForcibly()
    }

  private val _terminated: Task[ReturnCode] =
    Task.defer {
      process.returnCode.map(Task.pure)
        .getOrElse(ioTask {
          waitForProcessTermination(process)
        })
    }.memoize

  def duration = runningSince.elapsed

  def terminated: Task[ReturnCode] =
    _terminated

  final def sendProcessSignal(signal: ProcessSignal): Task[Unit] =
    Task.defer {
      if (signal != SIGKILL && !isWindows)
        destroy(force = false)
      else
        ifAlive("sendProcessSignal SIGKILL")(
          processConfiguration
            .toKillScriptCommandArgumentsOption(pidOption)
            .fold(kill)(args =>
              executeKillScript(args ++ pidOption.map(o => s"--pid=${o.string}"))
                .onErrorHandle(t => logger.error(
                  s"Cannot start kill script command '$args': ${t.toStringWithCauses}"))
                .tapEval(_ => kill))
        ).guarantee(Task {
          // The process may have terminated
          // while long running child processes inheriting the file handles
          // still use these.
          // So we forcibly close stdout and stderr.
          // The child processes write operations will fail with EPIPE or may block !!!
          // Let destroyForcibly try to close the handles (implementation dependent)
          onSigkill()
        })
    }

  private def executeKillScript(args: Seq[String]): Task[Unit] =
    ifAlive("executeKillScript")(
      if (isMac)
        Task {
          // TODO On MacOS, the kill script may kill a foreign process like the developers IDE
          logger.warn("Execution of kill script is suppressed on MacOS")
        }
      else
        Task.defer {
          logger.info("Executing kill script: " + args.mkString("  "))
          val processBuilder = new ProcessBuilder(args.asJava)
            .redirectOutput(INHERIT)
            .redirectError(INHERIT)
          processBuilder
            .startRobustly()
            .executeOn(iox.scheduler)
            .flatMap(onKillProcess =>
              ioTask {
                waitForProcessTermination(JavaProcess(onKillProcess))
              } >> Task {
                val exitCode = onKillProcess.exitValue
                val logLevel = if (exitCode == 0) LogLevel.Debug else LogLevel.Warn
                logger.log(logLevel, s"Kill script '${args(0)}' has returned exit code $exitCode")
              })
        })

  private def kill: Task[Unit] =
    destroy(force = true)

  private def destroy(force: Boolean): Task[Unit] =
    (process, pidOption) match {
      case (_: JavaProcess, Some(pid)) =>
        // Do not destroy with Java because Java closes stdout and stdin immediately,
        // not allowing a signal handler to write to stdout
        destroyWithUnixCommand(pid, force)

      case _ =>
        Task { destroyWithJava(force) }
    }

  private def destroyWithUnixCommand(pid: Pid, force: Boolean): Task[Unit] =
    ifAlive("destroyWithUnixCommand") {
      val argsPattern =
        if (force) processConfiguration.killWithSigkill
        else processConfiguration.killWithSigterm
      if (argsPattern.isEmpty)
        Task(destroyWithJava(force))
      else if (!argsPattern.contains("$pid")) {
        logger.error("Missing '$pid' in configured kill command")
        Task(destroyWithJava(force))
      } else {
        val args = argsPattern.map {
          case "$pid" => pid.number.toString
          case o => o
        }
        executeKillCommand(args)
          .flatMap { rc =>
            if (rc.isSuccess)
              Task.unit
            else Task {
              logger.warn(s"Could not kill with unix command: ${args.mkString(" ")} => $rc")
              destroyWithJava(force)
            }
          }
      }
    }

  private def ifAlive(label: String)(body: Task[Unit]): Task[Unit] =
    Task.defer(
      if (!process.isAlive)
        Task(logger.debug(s"$label: Process has already terminated"))
      else
        body)

  private def executeKillCommand(args: Seq[String]): Task[ReturnCode] =
    Task.defer {
      logger.info(args.mkString(" "))

      val processBuilder = new ProcessBuilder(args.asJava)
        .redirectOutput(INHERIT)  // TODO Pipe to stdout
        .redirectError(INHERIT)

      processBuilder
        .startRobustly()
        .executeOn(iox.scheduler)
        .flatMap(killProcess =>
          ioTask {
            waitForProcessTermination(JavaProcess(killProcess))
            ReturnCode(killProcess.exitValue)
          })
    }

  private def destroyWithJava(force: Boolean): Unit =
    if (force) {
      logger.debug("destroyForcibly")
      process.destroyForcibly()
    } else {
      logger.debug("destroy (SIGTERM)")
      process.destroy()
    }

  @TestOnly
  private[process] final def isAlive = process.isAlive

  final def stdin: OutputStream =
    process.stdin

  override def toString =
    process.toString
}

object RichProcess
{
  private val logger = Logger(getClass)

  def createStdFiles(directory: Path, id: String): Map[StdoutOrStderr, Path] =
    (StdoutOrStderr.values map { o => o -> newLogFile(directory, id, o) }).toMap

  private def waitForProcessTermination(process: Js7Process): ReturnCode = {
    logger.trace(s"waitFor $process ...")
    val rc = process.waitFor()
    logger.trace(s"waitFor $process exitCode=$rc")
    rc
  }

  def tryDeleteFile(file: Path) = tryDeleteFiles(file :: Nil)

  def tryDeleteFiles(files: Iterable[Path]): Boolean = {
    var allFilesDeleted = true
    for (file <- files) {
      try {
        logger.debug(s"Delete file '$file'")
        delete(file)
      }
      catch { case NonFatal(t) =>
        allFilesDeleted = false
        logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}")
      }
    }
    allFilesDeleted
  }
}
