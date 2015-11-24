package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.data.TaskServerConfiguration.Encoding
import com.sos.scheduler.engine.taskserver.task.process.Processes._
import com.sos.scheduler.engine.taskserver.task.process.RichProcess._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType, StdoutStderrTypes}
import java.io.{BufferedOutputStream, OutputStreamWriter}
import java.lang.ProcessBuilder.Redirect
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.nio.file.Path
import java.util.concurrent.TimeUnit.MILLISECONDS
import org.jetbrains.annotations.TestOnly
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise, blocking}
import scala.util.Try
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class RichProcess private(val processConfiguration: ProcessConfiguration, process: Process)
extends HasCloser with ClosedFuture {

  val pidOption = processToPidOption(process)
  private val logger = Logger.withPrefix(getClass, toString)
  /**
   * UTF-8 encoded stdin.
   */
  lazy val stdinWriter = new OutputStreamWriter(new BufferedOutputStream(stdin), UTF_8)
  private val terminatedPromise = Promise[Unit]()

  Future {
    terminatedPromise complete Try {
      blocking {
        waitForTermination()
        logger.info(s"Process ended with ${ReturnCode(process.exitValue)}")
      }
    }
  }

  logger.info(s"Process started")

  def terminated: Future[Unit] = terminatedPromise.future

  def sendProcessSignal(signal: ProcessSignal): Unit =
    signal match {
      case SIGTERM ⇒
        if (isWindows) throw new UnsupportedOperationException("SIGTERM is a Unix process signal and cannot be handled by Microsoft Windows")
        logger.debug("destroy")
        process.destroy()
      case SIGKILL ⇒
        processConfiguration.killScriptFileOption match {
          case Some(file) ⇒
            executeKillScriptFileThenKill(file) recover {
              case t ⇒ logger.error(s"Cannot start kill script '$file': $t")
            } onComplete { case _ ⇒
              killNow()
            }
          case None ⇒
            killNow()
        }
    }

  private def executeKillScriptFileThenKill(file: Path) = Future[Unit] {
    val args = toShellCommandArguments(file, List(processConfiguration.killScriptArgument))
    logger.info("Executing kill script: " + (args mkString ", "))
    val onKillProcess = new ProcessBuilder(args).start()
    val promise = Promise[Unit]()
      blocking { waitForProcessTermination(onKillProcess) }
      onKillProcess.exitValue match {
        case 0 ⇒
        case o ⇒ logger.warn(s"Kill script '$file' has returned exit code $o")
      }
      promise.success(())
    }

  private def killNow(): Unit = {
    if (process.isAlive) {
      logger.debug("destroyForcibly")
      process.destroyForcibly()
    }
  }

  @TestOnly
  private[task] def isAlive = process.isAlive

  def waitForTermination(): ReturnCode = {
    waitForProcessTermination(process)
    ReturnCode(process.exitValue)
  }

  def stdin = process.getOutputStream

  override def toString = (processConfiguration.idStringOption ++ List(processToString(process, pidOption)) ++ processConfiguration.fileOption) mkString " "
}

object RichProcess {
  private val WaitForProcessPeriod = 100.ms
  private val logger = Logger(getClass)

  def startShellScript(
    processConfiguration: ProcessConfiguration = ProcessConfiguration(),
    name: String = "shell-script",
    scriptString: String): RichProcess =
  {
    val shellFile = newTemporaryShellFile(name)
    try {
      shellFile.write(scriptString, Encoding)
      val process = RichProcess.start(processConfiguration.copy(fileOption = Some(shellFile)), shellFile)
      process.closed.onComplete { _ ⇒ tryDeleteFiles(List(shellFile)) }
      process.stdin.close() // Process gets an empty stdin
      process
    }
    catch { case NonFatal(t) ⇒
      shellFile.delete()
      throw t
    }
  }

  def start(processConfiguration: ProcessConfiguration, file: Path, arguments: Seq[String] = Nil): RichProcess = {
    import processConfiguration.{additionalEnvironment, stdFileMap}
    val processBuilder = new ProcessBuilder(toShellCommandArguments(file, arguments ++ processConfiguration.idArgumentOption))
    processBuilder.redirectOutput(toRedirect(stdFileMap.get(Stdout)))
    processBuilder.redirectError(toRedirect(stdFileMap.get(Stderr)))
    processBuilder.environment ++= additionalEnvironment
    logger.info("Start process " + (arguments map { o ⇒ s"'$o'" } mkString ", "))
    val process = processBuilder.start()
    new RichProcess(processConfiguration, process)
  }

  private def toRedirect(pathOption: Option[Path]) = pathOption map { o ⇒ Redirect.to(o) } getOrElse INHERIT

  def createStdFiles(directory: Path, name: String): Map[StdoutStderrType, Path] = (StdoutStderrTypes map { o ⇒ o → newLogFile(directory, name, o) }).toMap

  private def waitForProcessTermination(process: Process): Unit = {
    logger.debug(s"waitFor ${processToString(process)} ...")
    while (!process.waitFor(WaitForProcessPeriod.toMillis, MILLISECONDS)) {}
    logger.debug(s"waitFor ${processToString(process)} exitCode=${process.exitValue}")
  }

  def tryDeleteFiles(files: Iterable[Path]): Unit =
    for (file ← files) {
      try {
        logger.debug(s"Delete file '$file'")
        delete(file)
      }
      catch { case NonFatal(t) ⇒ logger.error(t.toString) }
    }
}
