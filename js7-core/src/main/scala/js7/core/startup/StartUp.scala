package js7.core.startup

import java.io.File
import java.nio.file.Path
import java.time.LocalDateTime
import js7.base.system.SystemInformation.totalPhysicalMemory
import js7.common.process.ProcessPidRetriever.maybeOwnPid
import js7.common.scalautil.Logger
import js7.common.system.JavaInformations
import js7.common.system.OperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
import js7.common.utils.ByteUnits.toKiBGiB
import monix.execution.atomic.AtomicBoolean

/**
  * @author Joacim Zschimmer
  */
object StartUp
{
  private val logger = Logger(getClass)
  private val classPathLogged = AtomicBoolean(false)

  Halt  // Initialize class and object for maybe quicker emergency stop

  /** Log Java version, config and data directory, and classpath. */
  def logStartUp(configDir: Path, dataDir: Option[Path]): Unit = {
    logger.info(
      s"Java " + JavaInformations.implementationVersion + " " +
      "(" + toKiBGiB(sys.runtime.maxMemory) + ") · " +
      sys.props("os.name") + distributionNameAndVersionOption.fold("")(o => s" ($o)") + " · " +
      cpuModel.fold("")(o => s"$o ") + "(" + sys.runtime.availableProcessors + " threads)" +
      totalPhysicalMemory.fold("")(o => " " + toKiBGiB(o)) +
      " · " +
      (maybeOwnPid.fold("")(pid => s"pid=${pid.number} ")) +
      (if (hostname.nonEmpty) s"host=$hostname " else "") +
      s"config=$configDir " +
        dataDir.fold("")("data=".+))

    if (!classPathLogged.getAndSet(true)) {  // Log only once (for tests running controller and agents in same JVM)
      val paths = sys.props("java.class.path") split File.pathSeparator filter (_.nonEmpty)
      logger.debug(Logger.Java, s"Classpath contains ${paths.size} libraries:")
      for (o <- paths) {
        logger.debug(Logger.Java, s"Classpath $o")
      }
    }
    logger.whenTraceEnabled {
      logger.debug("Logger TRACE enabled")
    }
  }

  def printlnWithClockIgnoringException(line: String) =
    try printlnWithClock(line)
    catch { case _: Throwable => }

  def printlnWithClock(line: String) =
    System.err.println(s"${LocalDateTime.now.toString.replace('T', ' ')} $line")
}
