package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.log.LogLevel
import com.sos.jobscheduler.common.log.LogLevel.LevelScalaLogger
import com.sos.jobscheduler.common.process.Processes._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.ioFuture
import com.sos.jobscheduler.common.scalautil.{ClosedFuture, HasCloser, IOExecutor, Logger}
import com.sos.jobscheduler.common.system.OperatingSystem._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.taskserver.task.process.RichProcess._
import java.io.{BufferedOutputStream, OutputStream, OutputStreamWriter}
import java.lang.ProcessBuilder.Redirect
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.delete
import java.nio.file.Path
import java.time.Instant
import java.time.Instant.now
import org.jetbrains.annotations.TestOnly
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
class RichProcess protected[process](val processConfiguration: ProcessConfiguration, process: Process, argumentsForLogging: Seq[String])
  (implicit iox: IOExecutor, ec: ExecutionContext)
extends HasCloser with ClosedFuture {

  val startedAt = Instant.now
  val pidOption: Option[Pid] = processToPidOption(process)
  private val logger = Logger.withPrefix[RichProcess](toString)
  /**
   * UTF-8 encoded stdin.
   */
  lazy val stdinWriter = new OutputStreamWriter(new BufferedOutputStream(stdin), UTF_8)

  private lazy val _terminated: Future[ReturnCode] =
    if (process.isAlive)
      ioFuture {
        waitForProcessTermination(process)
      }
    else
      Future.successful(ReturnCode(process.exitValue))

  //logger.debug(s"Process started " + (argumentsForLogging map { o ⇒ s"'$o'" } mkString ", "))

  def duration = now - startedAt

  def terminated: Future[ReturnCode] =
    _terminated

  final def sendProcessSignal(signal: ProcessSignal): Unit =
    if (process.isAlive) {
      signal match {
        case SIGTERM ⇒
          if (isWindows) throw new UnsupportedOperationException("SIGTERM is a Unix process signal and cannot be handled by Microsoft Windows")
          logger.info("destroy (SIGTERM)")
          process.destroy()
        case SIGKILL ⇒
          processConfiguration.toKillScriptCommandArgumentsOption(pidOption) match {
            case Some(args) ⇒
              val pidArgs = pidOption map { o ⇒ s"-pid=${o.string}" }
              executeKillScript(args ++ pidArgs) recover {
                case t ⇒ logger.error(s"Cannot start kill script command '$args': $t")
              } onComplete { _ ⇒
                killNow()
              }
            case None ⇒
              killNow()
          }
      }
    }

  private def executeKillScript(args: Seq[String]): Future[Completed] =
    if (isMac) {
      logger.warn("Execution of kill script is suppressed on MacOS")  // TODO On MacOS, the kill script may kill a foreign process like the developers IDE
      Future.successful(Completed)
    } else {
      logger.info("Executing kill script: " + args.mkString("  "))
      val onKillProcess = new ProcessBuilder(args.asJava).redirectOutput(INHERIT).redirectError(INHERIT).start()
      ioFuture {
        waitForProcessTermination(onKillProcess)
        val exitCode = onKillProcess.exitValue
        logger.log(if (exitCode == 0) LogLevel.Debug else LogLevel.Warn, s"Kill script '${args(0)}' has returned exit code $exitCode")
        Completed
      }
    }

  private def killNow(): Unit = {
    if (process.isAlive) {
      logger.info("destroyForcibly" + (if (!isWindows) " (SIGKILL)" else ""))
      process.destroyForcibly()
    }
  }

  @TestOnly
  private[task] final def isAlive = process.isAlive

  final def stdin: OutputStream = process.getOutputStream

  override def toString = Some(processToString(process, pidOption)) ++ processConfiguration.agentTaskIdOption mkString " "
}

object RichProcess {
  private val logger = Logger(getClass)

  def start(processConfiguration: ProcessConfiguration, file: Path, arguments: Seq[String] = Nil)
    (implicit iox: IOExecutor, ec: ExecutionContext): RichProcess =
  {
    val process = startProcessBuilder(processConfiguration, file, arguments) { _.startRobustly() }
    new RichProcess(processConfiguration, process, argumentsForLogging = file.toString +: arguments)
  }

  private[process] def startProcessBuilder(processConfiguration: ProcessConfiguration, file: Path, arguments: Seq[String] = Nil)
      (start: ProcessBuilder ⇒ Process): Process = {
    import processConfiguration.{additionalEnvironment, stdFileMap}
    val processBuilder = new ProcessBuilder(toShellCommandArguments(file, arguments ++ processConfiguration.idArgumentOption).asJava)
    processBuilder.redirectOutput(toRedirect(stdFileMap.get(Stdout)))
    processBuilder.redirectError(toRedirect(stdFileMap.get(Stderr)))
    processBuilder.environment.putAll(additionalEnvironment.asJava)
    start(processBuilder)
  }

  private def toRedirect(pathOption: Option[Path]) = pathOption map { o ⇒ Redirect.to(o) } getOrElse INHERIT

  def createStdFiles(directory: Path, id: String): Map[StdoutOrStderr, Path] =
    (StdoutOrStderr.values map { o ⇒ o → newLogFile(directory, id, o) }).toMap

  private def waitForProcessTermination(process: Process): ReturnCode = {
    logger.trace(s"waitFor ${processToString(process)} ...")
    val returnCode = blocking {
      process.waitFor()
    }
    logger.trace(s"waitFor ${processToString(process)} exitCode=${process.exitValue}")
    ReturnCode(returnCode)
  }

  def tryDeleteFile(file: Path) = tryDeleteFiles(file :: Nil)

  def tryDeleteFiles(files: Iterable[Path]): Boolean = {
    var allFilesDeleted = true
    for (file ← files) {
      try {
        logger.debug(s"Delete files '$file'")
        delete(file)
      }
      catch { case NonFatal(t) ⇒
        allFilesDeleted = false
        logger.warn(s"Cannot delete file '$file': ${t.toStringWithCauses}")
      }
    }
    allFilesDeleted
  }
}
