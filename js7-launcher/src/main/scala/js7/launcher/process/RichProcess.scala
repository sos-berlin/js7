package js7.launcher.process

import java.io.OutputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, Pid, ProcessSignal, ReturnCode, StdoutOrStderr}
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.system.OperatingSystem.{isMac, isWindows}
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.process.RichProcess.*
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
class RichProcess protected[process](
  val processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit iox: IOExecutor):

  private val runningSince = now
  val pidOption: Option[Pid] = process.pid
  @volatile private var _isKilling = false
  private val logger = Logger.withPrefix[this.type](toString)

  protected final def isKilling = _isKilling

  protected def onSigkill(): Unit =
    if process.isAlive then
      logger.debug("destroyForcibly")
      process.destroyForcibly()

  private val _terminated: Task[ReturnCode] =
    Task.defer {
      process.returnCode.map(Task.pure)
        .getOrElse(iox(Task {
          waitForProcessTermination(process)
        }))
    }.memoize

  def duration = runningSince.elapsed

  def terminated: Task[ReturnCode] =
    _terminated

  final def sendProcessSignal(signal: ProcessSignal): Task[Unit] =
    Task.defer:
      if signal != SIGKILL && !isWindows then
        destroy(force = false)
      else
        ifAlive("sendProcessSignal SIGKILL")(
          processConfiguration
            .toKillScriptCommandArgumentsOption(pidOption)
            .fold(kill) { args =>
              _isKilling = true
              executeKillScript(args ++ pidOption.map(o => s"--pid=${o.string}"))
                .onErrorHandle(t => logger.error(
                  s"Cannot start kill script command '$args': ${t.toStringWithCauses}"))
                .tapEval(_ => kill)
            }
        ).guarantee(Task {
          // The process may have terminated
          // while long running child processes inheriting the file handles
          // still use these.
          // So we forcibly close stdout and stderr.
          // The child processes write operations will fail with EPIPE or may block !!!
          // Let destroyForcibly try to close the handles (implementation dependent)
          onSigkill()
        })

  private def executeKillScript(args: Seq[String]): Task[Unit] =
    ifAlive("executeKillScript")(
      if isMac then
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
              iox(Task {
                waitForProcessTermination(JavaProcess(onKillProcess))
              }) >> Task {
                val exitCode = onKillProcess.exitValue
                val logLevel = if exitCode == 0 then LogLevel.Debug else LogLevel.Warn
                logger.log(logLevel, s"Kill script '${args(0)}' has returned exit code $exitCode")
              })
        })

  private def kill: Task[Unit] =
    destroy(force = true)

  private def destroy(force: Boolean): Task[Unit] =
    (process, pidOption) match
      case (_: JavaProcess, Some(pid)) =>
        // Do not destroy with Java because Java closes stdout and stdin immediately,
        // not allowing a signal handler to write to stdout
        destroyWithUnixCommand(pid, force)

      case _ =>
        Task { destroyWithJava(force) }

  private def destroyWithUnixCommand(pid: Pid, force: Boolean): Task[Unit] =
    ifAlive("destroyWithUnixCommand"):
      val argsPattern =
        if isWindows then processConfiguration.killForWindows
        else if force then processConfiguration.killWithSigkill
        else processConfiguration.killWithSigterm
      if argsPattern.isEmpty then
        Task(destroyWithJava(force))
      else if !argsPattern.contains("$pid") then
        logger.error("Missing '$pid' in configured kill command")
        Task(destroyWithJava(force))
      else
        val args = argsPattern.map:
          case "$pid" => pid.number.toString
          case o => o
        executeKillCommand(args)
          .flatMap { rc =>
            if rc.isSuccess then
              Task.unit
            else Task:
              logger.warn(s"Could not kill with system command: ${args.mkString(" ")} => $rc")
              destroyWithJava(force)
          }

  private def ifAlive(label: String)(body: Task[Unit]): Task[Unit] =
    Task.defer(
      if !process.isAlive then
        Task(logger.debug(s"$label: Process has already terminated"))
      else
        body)

  private def executeKillCommand(args: Seq[String]): Task[ReturnCode] =
    Task.defer:
      logger.info(args.mkString(" "))

      val processBuilder = new ProcessBuilder(args.asJava)
        .redirectOutput(INHERIT)  // TODO Pipe to stdout
        .redirectError(INHERIT)

      processBuilder
        .startRobustly()
        .executeOn(iox.scheduler)
        .flatMap(killProcess =>
          iox(Task {
            waitForProcessTermination(JavaProcess(killProcess))
            ReturnCode(killProcess.exitValue)
          }))

  private def destroyWithJava(force: Boolean): Unit =
    if force then
      logger.debug("destroyForcibly")
      process.destroyForcibly()
    else
      logger.debug("destroy (SIGTERM)")
      process.destroy()

  @TestOnly
  private[process] final def isAlive = process.isAlive

  final def stdin: OutputStream =
    process.stdin

  override def toString =
    process.toString


object RichProcess:
  private val logger = Logger[this.type]

  def createStdFiles(directory: Path, id: String): Map[StdoutOrStderr, Path] =
    (StdoutOrStderr.values map { o => o -> newLogFile(directory, id, o) }).toMap

  private def waitForProcessTermination(process: Js7Process): ReturnCode =
    logger.trace(s"waitFor $process ...")
    val rc = process.waitFor()
    logger.trace(s"waitFor $process exitCode=$rc")
    rc

  def tryDeleteFile(file: Path) = tryDeleteFiles(file :: Nil)

  def tryDeleteFiles(files: Iterable[Path]): Boolean =
    var allFilesDeleted = true
    for file <- files do
      try
        logger.debug(s"Delete file '$file'")
        delete(file)
      catch { case NonFatal(t) =>
        allFilesDeleted = false
        logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}")
      }
    allFilesDeleted
