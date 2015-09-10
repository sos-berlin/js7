package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import java.nio.file.Files._
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.attribute.PosixFilePermissions._
import scala.collection.immutable

object Processes {

  def processToString(process: Process): String = processToString(process, processToPidOption(process))

  def processToString(process: Process, pid: Option[Pid]) = pid map { _.toString } getOrElse process.toString

  def processToPidOption(process: Process): Option[Pid] = ProcessesJava8pid.processToPid(process)

  final case class Pid(value: Long)



  def newTemporaryShellFile(name: String): Path = OS.newTemporaryShellFile(name)

  def newTemporaryOutputFile(name: String, outerr: StdoutStderrType): Path = OS.newTemporaryOutputFile(name, outerr)

  def toShellCommandArguments(file: Path, arguments: Seq[String] = Nil): immutable.Seq[String] = OS.toShellCommandArguments(file, arguments)

  def toShellCommandArguments(argument: String): immutable.Seq[String] = OS.toShellCommandArguments(argument)

  private val OS: OperatingSystemSpecific = if (isWindows) OperatingSystemSpecific.Windows else OperatingSystemSpecific.Unix

  private sealed trait OperatingSystemSpecific {
    def newTemporaryShellFile(name: String): Path
    def newTemporaryOutputFile(name: String, outerr: StdoutStderrType): Path
    def toShellCommandArguments(file: Path, arguments: Seq[String] = Nil): immutable.Seq[String]
    def toShellCommandArguments(argument: String): immutable.Seq[String]

    protected final def filenamePrefix(name: String) = s"JobScheduler-Agent-$name-"
  }

  private object OperatingSystemSpecific {
    private[Processes] object Unix extends OperatingSystemSpecific {
      private val shellFileAttribute = asFileAttribute(PosixFilePermissions fromString "rwx------")
      private val outputFileAttribute = asFileAttribute(PosixFilePermissions fromString "rw-------")

      def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".sh", shellFileAttribute)

      def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}", s".$outerr", outputFileAttribute)

      /** @return /bin/sh, -c, 'FILE' "$@", arg0, arg1, arg2, ... */
      def toShellCommandArguments(file: Path, arguments: Seq[String]) =
        Vector("/bin/sh", "-c", s"""${quote(file)} "$$@"""") ++ List(file.toString) ++ arguments  // "-c" respects shebang "#!"

      def toShellCommandArguments(argument: String) = Vector("/bin/sh", "-c", argument)

      private def quote(path: Path) = {
        val p = path.toString
        if (p contains '\'') throw new IllegalArgumentException("Single quote not possible in shell script path")
        s"'$p'"
      }
    }

    private[Processes] object Windows extends OperatingSystemSpecific {
      private val CharacterIsNotAllowed = Set(' ', '"')
      private val Cmd: String = sys.env.get("ComSpec") orElse sys.env.get("COMSPEC" /*cygwin*/) getOrElse """C:\Windows\system32\cmd.exe"""

      def newTemporaryShellFile(name: String) = createTempFile(filenamePrefix(name), ".cmd")

      def newTemporaryOutputFile(name: String, outerr: StdoutStderrType) = createTempFile(s"${filenamePrefix(name)}$outerr-", ".log")

      def toShellCommandArguments(file: Path, arguments: Seq[String]) = {
        // This does not work properly for all argument strings !!!
        // See https://stackoverflow.com/questions/4094699
        if (arguments exists { _ exists CharacterIsNotAllowed }) throw new IllegalArgumentException("Unallowed character in Windows command line arguments")
        Vector(Cmd, "/C", file.toString) ++ arguments
      }

      def toShellCommandArguments(argument: String) = Vector(Cmd, "/C", argument)
    }
  }
}
