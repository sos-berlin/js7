package com.sos.scheduler.engine.common.process

import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.process.OperatingSystemSpecific.OS
import com.sos.scheduler.engine.common.process.Processes.RobustlyStartProcess.TextFileBusyIOException
import com.sos.scheduler.engine.common.process.StdoutStderr.StdoutStderrType
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.io.IOException
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import java.time.Duration
import scala.collection.immutable

object Processes {
  private val logger = Logger(getClass)

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

  /**
    * Including dot.
    * For example ".sh" or ".cmd".
    */
  val ShellFileExtension = OS.shellFileExtension

  val ShellFileAttributes: immutable.Seq[FileAttribute[java.util.Set[_]]] = OS.shellFileAttributes

  def newTemporaryShellFile(name: String): Path = OS.newTemporaryShellFile(name)

  def newLogFile(directory: Path, name: String, outerr: StdoutStderrType): Path = OS.newLogFile(directory, name, outerr)

  def directShellCommandArguments(argument: String): immutable.Seq[String] = OS.directShellCommandArguments(argument)

  implicit class RobustlyStartProcess(val delegate: ProcessBuilder) extends AnyVal {
    /**
      * Like ProcessBuilder.start, but retries after IOException("error=26, Text file busy").
      *
      * @see https://change.sos-berlin.com/browse/JS-1581
      * @see https://bugs.openjdk.java.net/browse/JDK-8068370
      */
    def startRobustly(durations: Iterator[Duration] = RobustlyStartProcess.DefaultDurations.iterator): Process =
      try delegate.start()
      catch {
        case TextFileBusyIOException(e) if durations.hasNext ⇒
          logger.warn(s"Retrying process start after error: $e")
          sleep(durations.next())
          startRobustly(durations)
      }
  }

  private[process] object RobustlyStartProcess {
    private val DefaultDurations = List(10.ms, 50.ms, 500.ms, 1440.ms) ensuring { o ⇒ (o map { _.toMillis }).sum.ms == 2.s }

    object TextFileBusyIOException {
      private val matchesError26 = """.*\berror=26\b.*""".r.pattern.matcher _
      def unapply(e: IOException): Option[IOException] = matchesError26(e.getMessage).matches option e
    }
  }
}
