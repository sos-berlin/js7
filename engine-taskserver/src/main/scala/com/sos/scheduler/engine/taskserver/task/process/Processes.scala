package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import scala.collection.immutable

object Processes {

  def processToString(process: Process): String = processToString(process, processToPidOption(process))

  def processToString(process: Process, pid: Option[Pid]) = pid map { _.toString } getOrElse process.toString

  def processToPidOption(process: Process): Option[Pid] = ProcessesJava8pid.processToPid(process)

  final case class Pid(number: Long) {
    def string = number.toString
  }

  /**
   * Builds an argument list for [[ProcessBuilder]].
   */
  def toShellCommandArguments(file: Path, arguments: Seq[String] = Nil): immutable.Seq[String] = Vector(file.toString) ++ arguments


  // Shortcuts for operating system specific methods
  import OperatingSystemSpecific.OS

  def newTemporaryShellFile(name: String): Path = OS.newTemporaryShellFile(name)

  def newLogFile(directory: Path, name: String, outerr: StdoutStderrType): Path = OS.newLogFile(directory, name, outerr)

  def directShellCommandArguments(argument: String): immutable.Seq[String] = OS.directShellCommandArguments(argument)

  def shellFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]] = OS.shellFileAttributes
}
