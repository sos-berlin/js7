package js7.launcher.process

import cats.effect.IO
import java.io.OutputStream
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, Pid, ProcessSignal, ReturnCode}
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.system.OperatingSystem.{isMac, isWindows}
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
abstract class RichProcess protected[process](
  val processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (using IOExecutor):

  private val runningSince = now
  val pidOption: Option[Pid] = process.pid
  @volatile private var _isKilling = false
  private val logger = Logger.withPrefix[this.type](toString)

  protected final def isKilling = _isKilling

  protected def onSigkill: IO[Unit]

  private val _terminated: IO[ReturnCode] =
    memoize:
      IO.defer:
        process.returnCode.map(IO.pure)
          .getOrElse:
            waitForProcessTermination(process)

  def duration: FiniteDuration = runningSince.elapsed

  def terminated: IO[ReturnCode] =
    _terminated

  final def sendProcessSignal(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      if signal != SIGKILL && !isWindows then
        destroy(force = false)
      else
        ifAliveForKilling("sendProcessSignal SIGKILL"):
          processConfiguration
            .toKillScriptCommandArgumentsOption(pidOption)
            .fold(destroy(force = true)): args =>
              _isKilling = true
              executeKillScript(args ++ pidOption.map(o => s"--pid=${o.string}"))
                .handleError(t => logger.error(
                  s"Cannot start kill script command '$args': ${t.toStringWithCauses}"))
                .<*(destroy(force = true))
        .guarantee:
          // The process may have terminated while long running child processes have inherited the
          // file handles and still use them.
          // So we forcibly close stdout and stderr.
          // The child processes write operations will fail with EPIPE or may block !!!
          // Let destroyForcibly try to close the handles (implementation dependent)
          onSigkill

  private def executeKillScript(args: Seq[String]): IO[Unit] =
    ifAliveForKilling("executeKillScript"):
      if isMac then
        IO:
          // TODO On macOS, the kill script may kill a foreign process like the developers IDE
          logger.warn("Execution of kill script is suppressed on macOS")
      else
        IO.defer:
          logger.info("Executing kill script: " + args.mkString("  "))
          val processBuilder = new ProcessBuilder(args.asJava)
            .redirectOutput(INHERIT)
            .redirectError(INHERIT)
          processBuilder
            .startRobustly()
            .flatMap: killProcess =>
              waitForProcessTermination(JavaProcess(killProcess))
            .flatMap(returnCode => IO:
              val logLevel = if returnCode.isSuccess then LogLevel.Debug else LogLevel.Warn
              logger.log(logLevel, s"Kill script '${args.head}' has returned $returnCode"))

  private def destroy(force: Boolean): IO[Unit] =
    (process, pidOption) match
      case (_: JavaProcess, Some(pid)) =>
        // Do not destroy with Java because Java closes stdout and stdin immediately,
        // not allowing a signal handler to write to stdout
        destroyWithUnixCommand(pid, force)

      case _ =>
        IO { destroyWithJava(force) }

  private def destroyWithUnixCommand(pid: Pid, force: Boolean): IO[Unit] =
    ifAliveForKilling("destroyWithUnixCommand"):
      val argsPattern =
        if isWindows then processConfiguration.killForWindows
        else if force then processConfiguration.killWithSigkill
        else processConfiguration.killWithSigterm
      if argsPattern.isEmpty then
        IO(destroyWithJava(force))
      else if !argsPattern.contains("$pid") then
        logger.error(s"Missing '$pid' in configured kill command")
        IO(destroyWithJava(force))
      else
        val args = argsPattern.map:
          case "$pid" => pid.number.toString
          case o => o
        executeKillCommand(args)
          .flatMap: rc =>
            if rc.isSuccess then
              IO.unit
            else IO:
              logger.warn(s"Could not kill with system command: ${args.mkString(" ")} => $rc")
              destroyWithJava(force)

  private def ifAliveForKilling(label: String)(body: IO[Unit]): IO[Unit] =
    IO.defer:
      if !process.isAlive then
        IO(logger.info(s"$label: 🚫 Not killed because process has already terminated with ${
          process.returnCode.fold("?")(_.pretty(isWindows))}"))
      else
        body

  private def executeKillCommand(args: Seq[String]): IO[ReturnCode] =
    IO.defer:
      logger.info(args.mkString(" "))
      ProcessBuilder(args.asJava)
        .redirectOutput(INHERIT)  // TODO Pipe to stdout
        .redirectError(INHERIT)
        .startRobustly()
        .flatMap: killProcess =>
          waitForProcessTermination(JavaProcess(killProcess))

  private def destroyWithJava(force: Boolean): Unit =
    if force then
      logger.debug("destroyForcibly")
      process.destroyForcibly()
    else
      logger.debug("destroy (SIGTERM)")
      process.destroy()

  @TestOnly
  private[process] final def isAlive = process.isAlive

  private def waitForProcessTermination(process: Js7Process): IO[ReturnCode] =
    IOExecutor.interruptible:
      logger.traceCallWithResult("waitFor", process):
        process.waitFor()

  final def stdin: OutputStream =
    process.stdin

  override def toString: String =
    process.toString


object RichProcess:
  private val logger = Logger[this.type]

  def tryDeleteFile(file: Path): Boolean =
    tryDeleteFiles(file :: Nil)

  def tryDeleteFiles(files: Iterable[Path]): Boolean =
    var allFilesDeleted = true
    for file <- files do
      try
        logger.debug(s"Delete file '$file'")
        delete(file)
      catch case NonFatal(t) =>
        allFilesDeleted = false
        logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}")
    allFilesDeleted
