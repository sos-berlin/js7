package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.RichProcess._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType, StdoutStderrTypes}
import java.io.{BufferedOutputStream, OutputStreamWriter}
import java.lang.ProcessBuilder.Redirect
import java.lang.ProcessBuilder.Redirect.INHERIT
import java.nio.charset.StandardCharsets._
import java.nio.file.Files.createTempFile
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.PosixFilePermissions._
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import org.jetbrains.annotations.TestOnly
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class RichProcess private(val processConfiguration: ProcessConfiguration, process: Process)
extends HasCloser with ClosedFuture {

  private val logger = Logger.withPrefix(getClass, toString)
  lazy val stdinWriter = new OutputStreamWriter(new BufferedOutputStream(stdin), UTF_8)

  def kill() = process.destroyForcibly()

  def sendProcessSignal(signal: ProcessSignal): Unit =
    signal match {
      case SIGTERM ⇒
        if (isWindows) throw new UnsupportedOperationException("SIGTERM is a Unix process signal and cannot be handled by Microsoft Windows")
        logger.debug("destroy")
        process.destroy()
      case SIGKILL ⇒
        processConfiguration.killScriptFileOption match {
          case Some(onKillScriptFile) ⇒
            try {
              val args = OS.toShellCommandArguments(onKillScriptFile, List(processConfiguration.killScriptArgument))
              val onKillProcess = new ProcessBuilder(args).start()
              Future {
                blocking {
                  waitForProcessTermination(onKillProcess)
                }
                killNow()
              }
            } catch {
              case NonFatal(t) ⇒
                killNow()
                throw t
            }
          case None ⇒
            killNow()
        }
    }

  private def killNow(): Unit = {
    logger.debug("destroyForcibly")
    process.destroyForcibly()
  }

  @TestOnly
  private[task] def isAlive = process.isAlive

  def waitForTermination(): ReturnCode = {
    logger.debug("waitForTermination ...")
    waitForProcessTermination(process)
    logger.debug(s"waitForTermination: terminated with exit code ${process.exitValue}")
    ReturnCode(process.exitValue)
  }

  def stdin = process.getOutputStream

  override def toString = (List(process) ++ processConfiguration.fileOption ++ processConfiguration.idStringOption) mkString " "
}

object RichProcess {
  private val WaitForProcessPeriod = 100.ms
  private val logger = Logger(getClass)

  def startShellScript(
    processConfiguration: ProcessConfiguration = ProcessConfiguration(),
    name: String = "shell-script",
    scriptString: String): RichProcess =
  {
    val shellFile = OS.newTemporaryShellFile(name)
    try {
      shellFile.toFile.write(scriptString, ISO_8859_1)
      val process = RichProcess.start(processConfiguration.copy(fileOption = Some(shellFile)), OS.toShellCommandArguments(shellFile))
      process.closed.onComplete { case _ ⇒ tryDeleteFiles(List(shellFile)) }
      process.stdin.close() // Empty stdin
      process
    }
    catch { case NonFatal(t) ⇒
      shellFile.delete()
      throw t
    }
  }

  def start(processConfiguration: ProcessConfiguration, arguments: Seq[String]): RichProcess = {
    import processConfiguration.{additionalEnvironment, stdFileMap}
    val processBuilder = new ProcessBuilder(arguments ++ processConfiguration.idArgumentOption)
    processBuilder.redirectOutput(toRedirect(stdFileMap.get(Stdout)))
    processBuilder.redirectError(toRedirect(stdFileMap.get(Stderr)))
    processBuilder.environment ++= additionalEnvironment
    logger.debug("Start process " + (arguments map { o ⇒ s"'$o'" } mkString ", "))
    val process = processBuilder.start()
    new RichProcess(processConfiguration, process)
  }

  private def toRedirect(pathOption: Option[Path]) = pathOption map { o ⇒ Redirect.to(o) } getOrElse INHERIT

  def createTemporaryStdFiles(): Map[StdoutStderrType, Path] = (StdoutStderrTypes map { o ⇒ o → OS.newTemporaryOutputFile("sos", o) }).toMap

  private def waitForProcessTermination(process: Process): Unit = {
    while (!process.waitFor(WaitForProcessPeriod.toMillis, TimeUnit.MILLISECONDS)) {}   // Die waitFor-Implementierung fragt millisekündlich ab
  }

  final case class KillConfiguration(agentTaskId: AgentTaskId, onKillScriptFile: Path)

  val OS = if (isWindows) WindowsSpecific else UnixSpecific

  trait OperatingSystemSpecific {
    def newTemporaryShellFile(name: String): Path
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType): Path
    def toShellCommandArguments(file: Path, arguments: Seq[String] = Nil): immutable.Seq[String]
    protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name-"
  }

  private object UnixSpecific extends OperatingSystemSpecific {
    private val shellFileAttribute = asFileAttribute(PosixFilePermissions fromString "rwx------")
    private val outputFileAttribute = asFileAttribute(PosixFilePermissions fromString "rw-------")
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".sh", shellFileAttribute)
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-", s".$outerr", outputFileAttribute)
    def toShellCommandArguments(file: Path, arguments: Seq[String]) = Vector("/bin/sh", file.toString) ++ arguments
  }

  private object WindowsSpecific extends OperatingSystemSpecific {
    private val cmd = sys.env("ComSpec")  // C:\Windows\system32\cmd.exe
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".cmd")
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-$outerr-", ".log")
    def toShellCommandArguments(file: Path, arguments: Seq[String]) = Vector(cmd, "/C", file.toString) ++ arguments
  }

  def tryDeleteFiles(files: Iterable[Path]): Unit =
    for (file ← files) {
      try {
        logger.debug(s"Delete file '$file'")
        Files.delete(file)
      }
      catch { case NonFatal(t) ⇒ logger.error(t.toString) }
    }
}
