package js7.common.system.startup

import java.io.File
import java.time.Instant
import js7.base.io.process.ProcessPidRetriever.maybeOwnPid
import js7.base.log.Logger
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.ScalaUtils.syntax._
import js7.common.system.ServerOperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
import js7.common.system.SystemInformations.totalPhysicalMemory
import monix.execution.atomic.AtomicBoolean
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
object StartUp
{
  val runningSince = now
  val startedAt = Timestamp.now
  private var _isMain = false
  private val logger = Logger(getClass)
  private val classPathLogged = AtomicBoolean(false)

  def initializeMain(): Unit =
    _isMain = true

  def isMain = _isMain

  // Initialize class and object for possible quicker emergency stop
  Halt

  def logJavaSettings(): Unit = {
    if (!classPathLogged.getAndSet(true)) {  // Log only once (for tests running controller and agents in same JVM)
      val paths = sys.props("java.class.path").split(File.pathSeparator).filter(_.nonEmpty)
      logger.debug(Logger.Java, s"Classpath contains ${paths.length} entries:")
      for (o <- paths) {
        logger.debug(Logger.Java, s"Classpath $o")
      }
    }
    logger.whenTraceEnabled {
      logger.debug("Logger TRACE enabled")
    }
  }

  /** Log Java version, config and data directory, and classpath. */
  def startUpLine(): String =
    "Java " + sys.props.getOrElse("java.version", "") + " · " +
      sys.props.getOrElse("java.vm.name", sys.props.getOrElse("java.runtime.name", "vm")) + " " +
      sys.props.getOrElse("java.vm.version", "") + " " +
      "(" + toKiBGiB(sys.runtime.maxMemory) + ") · " +
      sys.props("os.name") + distributionNameAndVersionOption.fold("")(o => s" ($o)") + " · " +
      cpuModel.fold("")(o => s"$o ") + "(" + sys.runtime.availableProcessors + " threads)" +
      totalPhysicalMemory.fold("")(o => " " + toKiBGiB(o)) +
      " · " +
      maybeOwnPid.fold("")(pid => s"pid=${pid.number} ") +
      (hostname.nonEmpty ?? s"host=$hostname ")

  def printlnWithClockIgnoringException(line: String) =
    try printlnWithClock(line)
    catch { case _: Throwable => }

  def printlnWithClock(line: String): Unit =
    System.err.println(s"$nowString $line")

  def nowString =
    Instant.now.toString
      .replace('T', ' ')
      .take(23)/*ms*/
}
