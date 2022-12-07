package js7.common.system.startup

import java.io.File
import java.time.Instant
import js7.base.io.process.ProcessPidRetriever.maybeOwnPid
import js7.base.log.{CorrelId, Logger}
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.ScalaUtils.syntax.*
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
  private val classPathLogged = AtomicBoolean(false)

  def initializeMain(): Unit =
    _isMain = true

  def isMain = _isMain

  def logJavaSettings(): Unit = {
    // Do not initialize logging framework to early
    val logger = Logger[this.type]

    if (!classPathLogged.getAndSet(true)) {  // Log only once (for tests running controller and agents in same JVM)
      val paths = sys.props("java.class.path").split(File.pathSeparator).filter(_.nonEmpty)
      logger.debug(Logger.Java, s"Classpath contains ${paths.length} entries:")
      for (o <- paths) {
        logger.debug(Logger.Java, s"Classpath $o")
      }
    }
    if (CorrelId.isEnabled) {
      // Check isEnabled after some debug line has been logged,
      // because CorrelIds may be enabled automatically on the first use by Log4j.
      CorrelId("CorrelId").bind {
        logger.debug("Correlation IDs are enabled")
      }
    }
    logger.whenTraceEnabled {
      logger.debug("TRACE level logging is enabled")
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
