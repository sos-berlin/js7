package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.RichProcess._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType, StdoutStderrTypes}
import java.io.{BufferedOutputStream, OutputStreamWriter}
import java.nio.charset.Charset
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
import scala.util.control.NonFatal

/**
 * @param infoProgramFile only for information
 * @author Joacim Zschimmer
 */
final class RichProcess private(process: Process, infoProgramFile: Path, stdFileMap: Map[StdoutStderrType, Path], fileEncoding: Charset)
extends HasCloser with ClosedFuture {

  lazy val stdinWriter = new OutputStreamWriter(new BufferedOutputStream(stdin), UTF_8)

  def kill() = process.destroyForcibly()

  def waitForTermination(): ReturnCode = {
    while (!process.waitFor(WaitForProcessPeriod.toMillis, TimeUnit.MILLISECONDS)) {}   // Die waitFor-Implementierung fragt millisekündlich ab
    logger.debug(s"Terminated with exit code ${process.exitValue}")
    ReturnCode(process.exitValue)
  }

  def stdin = process.getOutputStream

  override def toString = s"$process $infoProgramFile"

  @TestOnly
  def files: immutable.Seq[Path] = List(infoProgramFile) ++ stdFileMap.values
}

object RichProcess {
  private val WaitForProcessPeriod = 100.ms
  private val logger = Logger(getClass)

  def startShellScript(name: String, additionalEnvironment: immutable.Iterable[(String, String)], scriptString: String, stdFileMap: Map[StdoutStderrType, Path]): RichProcess = {
    val shellFile = OS.newTemporaryShellFile(name)
    try {
      shellFile.toFile.write(scriptString, OS.fileEncoding)
      val process = RichProcess.start(OS.toShellCommandArguments(shellFile), additionalEnvironment, stdFileMap, infoProgramFile = shellFile)
      process.closed.onComplete { case _ ⇒ tryDeleteFiles(List(shellFile)) }
      process.stdin.close() // Empty stdin
      process
    }
    catch { case NonFatal(t) ⇒
      shellFile.delete()
      throw t
    }
  }

  def start(arguments: Seq[String], additionalEnvironment: Iterable[(String, String)], stdFileMap: Map[StdoutStderrType, Path], infoProgramFile: Path): RichProcess = {
    val processBuilder = new ProcessBuilder(arguments)
    processBuilder.redirectOutput(stdFileMap(Stdout))
    processBuilder.redirectError(stdFileMap(Stderr))
    processBuilder.environment ++= additionalEnvironment
    logger.debug("Start process " + (arguments map { o ⇒ s"'$o'" } mkString ", "))
    val process = processBuilder.start()
    new RichProcess(process, infoProgramFile, stdFileMap, OS.fileEncoding)
  }

  def createTemporaryStdFiles(): Map[StdoutStderrType, Path] = (StdoutStderrTypes map { o ⇒ o → OS.newTemporaryOutputFile("sos", o) }).toMap

  private val OS = if (isWindows) WindowsSpecific else UnixSpecific

  private trait OperatingSystemSpecific {
    val fileEncoding: Charset
    def newTemporaryShellFile(name: String): Path
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType): Path
    def toShellCommandArguments(file: Path): immutable.Seq[String]
    protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name-"
  }

  private object UnixSpecific extends OperatingSystemSpecific {
    private val shellFileAttribute = asFileAttribute(PosixFilePermissions fromString "rwx------")
    private val outputFileAttribute = asFileAttribute(PosixFilePermissions fromString "rw-------")
    val fileEncoding = UTF_8  // Unix-Umgebungsvariable beachten??? LANG=de_DE.UTF_8
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".sh", shellFileAttribute)
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-", s".$outerr", outputFileAttribute)
    def toShellCommandArguments(file: Path) = Vector("/bin/sh", file.toString)
  }

  private object WindowsSpecific extends OperatingSystemSpecific {
    val fileEncoding = ISO_8859_1
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".cmd")
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-$outerr-", ".log")
    def toShellCommandArguments(file: Path) = Vector("""C:\Windows\System32\cmd.exe""", "/C", file.toString)
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
