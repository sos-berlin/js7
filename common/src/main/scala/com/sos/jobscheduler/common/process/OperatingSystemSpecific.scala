package com.sos.jobscheduler.common.process

import com.sos.jobscheduler.data.system.StdoutStderr.StdoutStderrType
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.OperatingSystem._
import java.nio.file.Files._
import java.nio.file.{FileAlreadyExistsException, Path}
import java.nio.file.attribute.PosixFilePermissions._
import java.nio.file.attribute.{FileAttribute, PosixFilePermissions}
import scala.collection.immutable
import OperatingSystemSpecific._

/**
  * @author Joacim Zschimmer
  */
private[process] sealed trait OperatingSystemSpecific {

  /**
    * Including dot.
    * For example ".sh" or ".cmd".
    */
  def shellFileExtension: String

  def shellFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]]

  def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), shellFileExtension, shellFileAttributes: _*)

  def newLogFile(directory: Path, name: String, outerr: StdoutStderrType) = {
    val file = directory resolve s"$name-$outerr.log"
    try createFile(file, outputFileAttributes: _*)
    catch { case t: FileAlreadyExistsException ⇒
      logger.debug(t.toString)  // Should rarely happen
    }
    file
  }

  protected def outputFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]]

  def directShellCommandArguments(argument: String): immutable.Seq[String]

  protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name-"
}

private object OperatingSystemSpecific {
  private val logger = Logger(getClass)

  private[process] val OS: OperatingSystemSpecific = if (isWindows) OperatingSystemSpecific.Windows else OperatingSystemSpecific.Unix

  private object Unix extends OperatingSystemSpecific {
    val shellFileExtension = ".sh"
    val shellFileAttributes = List(asFileAttribute(PosixFilePermissions fromString "rwx------"))
      .asInstanceOf[immutable.Seq[FileAttribute[java.util.Set[_]]]]
    val outputFileAttributes = List(asFileAttribute(PosixFilePermissions fromString "rw-------"))
      .asInstanceOf[immutable.Seq[FileAttribute[java.util.Set[_]]]]

    def directShellCommandArguments(argument: String) = Vector("/bin/sh", "-c", argument)
  }

  private object Windows extends OperatingSystemSpecific {
    private val Cmd: String = sys.env.get("ComSpec") orElse sys.env.get("COMSPEC" /*cygwin*/) getOrElse """C:\Windows\system32\cmd.exe"""
    val shellFileExtension = ".cmd"
    val shellFileAttributes = Nil
    val outputFileAttributes = Nil

    def directShellCommandArguments(argument: String) = Vector(Cmd, "/C", argument)
  }
}
