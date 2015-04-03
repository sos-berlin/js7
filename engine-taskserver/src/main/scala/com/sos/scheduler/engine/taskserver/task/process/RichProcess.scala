package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.{HasCloser, Logger}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaJoda._
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.RichProcess._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType, StdoutStderrTypes}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets._
import java.nio.file.Files.createTempFile
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.PosixFilePermissions._
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.util.control.NonFatal

/**
 * @param infoProgramFile only for information
 * @author Joacim Zschimmer
 */
final class RichProcess private(process: Process, infoProgramFile: Path, stdFiles: Map[StdoutStderrType, Path], fileEncoding: Charset)
extends HasCloser {

  private val promise = Promise[Unit]()
  def closed = promise.future

  private val firstLineCollector = new FirstLineCollector
  def firstStdoutLine = firstLineCollector.firstStdoutLine

  onClose {
    promise.success(())
  }

  def kill() = process.destroyForcibly()

  def waitForTermination(logOutputLine: String ⇒ Unit): ReturnCode = {
    autoClosing(new FilesLineCollector(Nil ++ stdFiles.values, fileEncoding)) { fileLogger ⇒
      def logOutputLines() = fileLogger.nextLinesIterator foreach { case (file, line) ⇒
        logOutputLine(line)
        firstLineCollector.apply(file, line)
      }
      while (!process.waitFor(LoggingPausePeriod.getMillis, TimeUnit.MILLISECONDS)) {     // Die waitFor-Implementierung fragt millisekündlich ab
        logOutputLines()
      }
      logger.debug(s"Terminated with exit code ${process.exitValue}")
      logOutputLines()
    }
    ReturnCode(process.exitValue)
  }

  override def toString = s"$process $infoProgramFile"

  def files: immutable.Seq[Path] = List(infoProgramFile) ++ stdFiles.values

  private class FirstLineCollector {
    private val maxLineNr = if (isWindows) 2 else 1  // Windows stdout may start with an empty first line
    private var stdoutLineNr = 0
    var firstStdoutLine = ""

    def apply(file: Path, line: String) =
      if (firstStdoutLine.isEmpty) {
        if (file == stdFiles(Stdout)) {
          stdoutLineNr += 1
          if (stdoutLineNr <= maxLineNr)
            firstStdoutLine = line
        }
      }
  }
}

object RichProcess {
  private val LoggingPausePeriod = 1.s
  private val logger = Logger(getClass)

  def startShellScript(name: String, additionalEnvironment: immutable.Iterable[(String, String)], scriptString: String): RichProcess = {
    val shellFile = OS.newTemporaryShellFile(name)
    try {
      shellFile.toFile.write(scriptString, OS.fileEncoding)
      val process = RichProcess.start(OS.toShellCommandArguments(shellFile), additionalEnvironment, infoProgramFile = shellFile)
      process.closed.onComplete { case _ ⇒ tryDeleteFiles(List(shellFile)) }
      process
    }
    catch { case NonFatal(t) ⇒
      shellFile.delete()
      throw t
    }
  }

  def start(arguments: Seq[String], additionalEnvironment: Iterable[(String, String)] = Nil, infoProgramFile: Path): RichProcess = {
    val stdFileMap = (StdoutStderrTypes map { o ⇒ o → OS.newTemporaryOutputFile("sos", o) }).toMap
    val processBuilder = new ProcessBuilder(arguments)
    processBuilder.redirectOutput(stdFileMap(Stdout))
    processBuilder.redirectError(stdFileMap(Stderr))
    processBuilder.environment ++= additionalEnvironment
    logger.debug("Start process " + (arguments map { o ⇒ s"'$o'" } mkString ", "))
    val process = processBuilder.start()
    process.getOutputStream.close() // Empty stdin
    val shellProcess = new RichProcess(process, infoProgramFile, stdFileMap, OS.fileEncoding)
    shellProcess.closed.onComplete { case _ ⇒ tryDeleteFiles(stdFileMap.values) }
    shellProcess
  }

  private val OS = if (isWindows) WindowsSpecific else UnixSpecific

  private trait OperatingSystemSpecific {
    val fileEncoding: Charset
    def newTemporaryShellFile(name: String): Path
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType): Path
    def toShellCommandArguments(file: Path): immutable.Seq[String]
    protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name"
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
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name) + "-", ".cmd")
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-$outerr-", ".log")
    def toShellCommandArguments(file: Path) = Vector("""C:\Windows\System32\cmd.exe""", "/C", file.toString)
  }

  private def tryDeleteFiles(files: Iterable[Path]): Unit =
    for (file ← files) {
      try {
        logger.debug(s"Delete file '$file'")
        Files.delete(file)
      }
      catch { case NonFatal(t) ⇒ logger.error(t.toString) }
    }
}
