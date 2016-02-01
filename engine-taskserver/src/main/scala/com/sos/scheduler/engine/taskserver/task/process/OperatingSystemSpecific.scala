package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import java.nio.file.Files._
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions._
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
private[process] sealed trait OperatingSystemSpecific {
  def newTemporaryShellFile(name: String): Path

  def newLogFile(directory: Path, name: String, outerr: StdoutStderrType) = {
    val file = directory resolve s"$name-$outerr.log"
    if (!exists(file)) {
      createFile(file, outputFileAttributes: _*)
    }
    file
  }

  def shellFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]]

  protected def outputFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]]

  def directShellCommandArguments(argument: String): immutable.Seq[String]

  protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name-"
}

private object OperatingSystemSpecific {

  private[process] val OS: OperatingSystemSpecific = if (isWindows) OperatingSystemSpecific.Windows else OperatingSystemSpecific.Unix

  private object Unix extends OperatingSystemSpecific {
    val shellFileAttributes = List(asFileAttribute(PosixFilePermissions fromString "rwx------"))
      .asInstanceOf[immutable.Seq[FileAttribute[java.util.Set[_]]]]
    val outputFileAttributes = List(asFileAttribute(PosixFilePermissions fromString "rw-------"))
      .asInstanceOf[immutable.Seq[FileAttribute[java.util.Set[_]]]]

    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".sh", shellFileAttributes: _*)

    def directShellCommandArguments(argument: String) = Vector("/bin/sh", "-c", argument)
  }

  private object Windows extends OperatingSystemSpecific {
    private val Cmd: String = sys.env.get("ComSpec") orElse sys.env.get("COMSPEC" /*cygwin*/) getOrElse """C:\Windows\system32\cmd.exe"""
    val shellFileAttributes = Nil
    val outputFileAttributes = Nil

    def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".cmd")

    def directShellCommandArguments(argument: String) = Vector(Cmd, "/C", argument)
  }
}
