package com.sos.jobscheduler.common.process

import com.sos.jobscheduler.base.generic.GenericLong
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.process.OperatingSystemSpecific.OS
import com.sos.jobscheduler.common.process.Processes.RobustlyStartProcess.TextFileBusyIOException
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.system.StdoutOrStderr
import java.io.IOException
import java.nio.file.Path
import java.nio.file.attribute.FileAttribute
import java.time.Duration
import scala.collection.immutable
import scala.concurrent.blocking

object Processes {
  private val logger = Logger(getClass)

  def processToString(process: Process): String = processToString(process, processToPidOption(process))

  def processToString(process: Process, pid: Option[Pid]) = pid map { _.toString } getOrElse process.toString

  def processToPidOption(process: Process): Option[Pid] = ProcessPidRetriever.processToPid(process)

  final case class Pid(number: Long) extends GenericLong {
    def string = number.toString
  }

  object Pid extends GenericLong.Companion[Pid]

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

  def newLogFile(directory: Path, name: String, outerr: StdoutOrStderr): Path = OS.newLogFile(directory, name, outerr)

  def directShellCommandArguments(argument: String): immutable.Seq[String] = OS.directShellCommandArguments(argument)

  implicit final class RobustlyStartProcess(private val delegate: ProcessBuilder) extends AnyVal {
    /**
      * Like ProcessBuilder.start, but retries after IOException("error=26, Text file busy").
      *
      * @see https://change.sos-berlin.com/browse/JS-1581
      * @see https://bugs.openjdk.java.net/browse/JDK-8068370
      */
    def startRobustly(durations: Iterator[Duration] = RobustlyStartProcess.DefaultDurations.iterator): Process =
      try delegate.start()
      catch {
        case TextFileBusyIOException(e) if durations.hasNext =>
          logger.warn(s"Retrying process start after error: $e")
          blocking {
            sleep(durations.next())
          }
          startRobustly(durations)
      }
  }

  private[process] object RobustlyStartProcess {
    private val DefaultDurations = List(10.ms, 50.ms, 500.ms, 1440.ms) ensuring { o => (o map { _.toMillis }).sum.ms == 2.s }

    object TextFileBusyIOException {
      private def matchesError26(o: String) = """.*\berror=26\b.*""".r.pattern.matcher(o)
      def unapply(e: IOException): Option[IOException] = matchesError26(e.getMessage).matches option e
    }
  }
}
