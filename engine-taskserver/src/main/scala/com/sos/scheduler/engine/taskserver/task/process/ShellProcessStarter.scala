package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout, StdoutStderrType, StdoutStderrTypes}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets._
import java.nio.file.Files.createTempFile
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.PosixFilePermissions._
import java.nio.file.{Files, Path}
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
object ShellProcessStarter {
  private val logger = Logger(getClass)

  def start(name: String, additionalEnvironment: Map[String, String], scriptString: String): ShellProcess = {
    val file = OS.newTemporaryShellFile(name)
    try {
      file.toFile.write(scriptString, OS.fileEncoding)
      val stdFileMap = (StdoutStderrTypes map { o ⇒ o → OS.newTemporaryOutputFile(name, o) }).toMap
      val processBuilder = new ProcessBuilder
      processBuilder.command(OS.toCommandArguments(file): _*)
      processBuilder.redirectOutput(stdFileMap(Stdout))
      processBuilder.redirectError(stdFileMap(Stderr))
      processBuilder.environment ++= additionalEnvironment
      val process = processBuilder.start()
      process.getOutputStream.close() // Empty stdin
      val shellProcess = new ShellProcess(process, file, stdFileMap, OS.fileEncoding)
      shellProcess.closedFuture.onComplete { case _ ⇒ tryDeleteFiles(List(file) ++ stdFileMap.values) }
      shellProcess
    }
    catch { case NonFatal(t) ⇒
      file.delete()
      throw t
    }
  }

  private val OS = if (isWindows) WindowsSpecific else UnixSpecific

  private trait OperatingSystemSpecific {
    val fileEncoding: Charset
    def newTemporaryShellFile(name: String): Path
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType): Path
    def toCommandArguments(file: Path): immutable.Seq[String]
    protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name"
  }

  private object UnixSpecific extends ShellProcessStarter.OperatingSystemSpecific {
    private val shellFileAttribute = asFileAttribute(PosixFilePermissions fromString "rwx------")
    private val outputFileAttribute = asFileAttribute(PosixFilePermissions fromString "rw-------")
    val fileEncoding = UTF_8  // Unix-Umgebungsvariable beachten??? LANG=de_DE.UTF_8
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".sh", shellFileAttribute)
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-", s".$outerr", outputFileAttribute)
    def toCommandArguments(file: Path) = Vector("/bin/sh", file.toString)
  }

  private object WindowsSpecific extends ShellProcessStarter.OperatingSystemSpecific {
    val fileEncoding = ISO_8859_1
    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name) + "-", ".cmd")
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}-$outerr-", ".log")
    def toCommandArguments(file: Path) = Vector("""C:\Windows\System32\cmd.exe""", "/C", file.toString)
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
