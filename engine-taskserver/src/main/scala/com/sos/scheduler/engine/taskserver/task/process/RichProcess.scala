package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.job.ReturnCode
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
class RichProcess protected[process](val processConfiguration: ProcessConfiguration, process: Process)
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

  final def terminated: Future[Unit] = terminatedPromise.future

  final def sendProcessSignal(signal: ProcessSignal): Unit =
    if (process.isAlive) {
      signal match {
        case SIGTERM ⇒
          if (isWindows) throw new UnsupportedOperationException("SIGTERM is a Unix process signal and cannot be handled by Microsoft Windows")
          logger.info("destroy (SIGTERM)")
          process.destroy()
        case SIGKILL ⇒
          processConfiguration.toCommandArgumentsOption match {
            case Some(args) ⇒
              executeKillScript(args) recover {
                case t ⇒ logger.error(s"Cannot start kill script command '$args': $t")
              } onComplete { case _ ⇒
                killNow()
              }
            case None ⇒
              killNow()
          }
      }
    }

  private def executeKillScript(args: Seq[String]) = Future[Unit] {
    logger.info("Executing kill script: " + (args mkString "  "))
    val onKillProcess = new ProcessBuilder(args).start()
    val promise = Promise[Unit]()
      blocking { waitForProcessTermination(onKillProcess) }
      onKillProcess.exitValue match {
        case 0 ⇒
        case o ⇒ logger.warn(s"Kill script '${args(0)}' has returned exit code $o")
      }
      promise.success(())
    }

  private def killNow(): Unit = {
    if (process.isAlive) {
      logger.info("destroyForcibly" + (if (!isWindows) " (SIGKILL)" else ""))
      process.destroyForcibly()
    }
  }

  @TestOnly
  private[task] final def isAlive = process.isAlive

  final def waitForTermination(): ReturnCode = {
    waitForProcessTermination(process)
    ReturnCode(process.exitValue)
  }

  final def stdin = process.getOutputStream

  override def toString = processConfiguration.agentTaskIdOption ++ List(processToString(process, pidOption)) ++ processConfiguration.fileOption mkString " "
}

object RichProcess {
  private val WaitForProcessPeriod = 100.ms
  private val logger = Logger(getClass)

  def start(processConfiguration: ProcessConfiguration, file: Path, arguments: Seq[String] = Nil): RichProcess = {
    val process = startProcessBuilder(processConfiguration, file, arguments) { _.start() }
    new RichProcess(processConfiguration, process)
  }

  private[process] def startProcessBuilder(processConfiguration: ProcessConfiguration, file: Path, arguments: Seq[String] = Nil)
      (start: ProcessBuilder ⇒ Process): Process = {
    import processConfiguration.{additionalEnvironment, stdFileMap}
    val processBuilder = new ProcessBuilder(toShellCommandArguments(file, arguments ++ processConfiguration.idArgumentOption))
    processBuilder.redirectOutput(toRedirect(stdFileMap.get(Stdout)))
    processBuilder.redirectError(toRedirect(stdFileMap.get(Stderr)))
    processBuilder.environment ++= additionalEnvironment
    logger.info("Start process " + (arguments map { o ⇒ s"'$o'" } mkString ", "))
    start(processBuilder)
  }

  private def toRedirect(pathOption: Option[Path]) = pathOption map { o ⇒ Redirect.to(o) } getOrElse INHERIT

  def createStdFiles(directory: Path, id: String): Map[StdoutStderrType, Path] = (StdoutStderrTypes map { o ⇒ o → newLogFile(directory, id, o) }).toMap

  private def waitForProcessTermination(process: Process): Unit = {
    logger.debug(s"waitFor ${processToString(process)} ...")
    while (!process.waitFor(WaitForProcessPeriod.toMillis, MILLISECONDS)) {}
    logger.debug(s"waitFor ${processToString(process)} exitCode=${process.exitValue}")
  }

  def tryDeleteFiles(files: Iterable[Path]): Boolean = {
    var allFilesDeleted = true
    for (file ← files) {
      try {
        logger.debug(s"Delete file '$file'")
        delete(file)
      }
      catch { case NonFatal(t) ⇒
        allFilesDeleted = false
        logger.error(t.toString)
      }
    }
    allFilesDeleted
  }
}
