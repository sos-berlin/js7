package js7.base.system.startup

import java.io.File
import java.time.ZonedDateTime
import js7.base.BuildInfo
import js7.base.log.{CorrelId, Logger}
import js7.base.system.ServerOperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
import js7.base.system.SystemInformations.totalPhysicalMemory
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Once
import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
object StartUp:
  val startedAt: Timestamp = Timestamp.now
  private var _isMain = false
  private val logClasspathOnce = Once()

  def initializeMain(): Unit =
    _isMain = true

  def isMain: Boolean =
    _isMain

  def logJavaSettings(): Unit =
    // Do not initialize logging framework to early
    val logger = Logger[this.type]

    logger.whenTraceEnabled:
      logger.debug("TRACE level logging is enabled")

    logClasspathOnce: // Log only once (for tests running controller and agents in same JVM)
      val paths = sys.props("java.class.path").split(File.pathSeparator).filter(_.nonEmpty)
      logger.debug(Logger.Java, s"Classpath contains ${paths.length} entries:")
      for o <- paths do
        logger.debug(Logger.Java, s"Classpath $o")

    if CorrelId.isEnabled then
      // Check isEnabled after some debug line has been logged,
      // because CorrelIds may be enabled automatically on the first use by Log4j.
      CorrelId("CorrelId").bind:
        logger.debug("Correlation IDs are enabled")

  def logStartUpLine(name: String): Unit =
    Logger[this.type].info(startUpLine(name))

  def startUpLine(name: String): String =
    s"""$name ${BuildInfo.prettyVersion} · ${startUpLine()}
       |${"━" * 80}""".stripMargin // Log a bar, in case we append to an existing log file

  /** Log Java version, config and data directory, and classpath. */
  def startUpLine(): String =
    ("Java " + sys.props.getOrElse("java.version", "") + " · " +
      sys.props.getOrElse("java.vm.name", sys.props.getOrElse("java.runtime.name", "vm")) + " " +
      sys.props.getOrElse("java.vm.version", "") + " " +
      "(" + toKiBGiB(sys.runtime.maxMemory) + ") · " +
      sys.props("os.name") + distributionNameAndVersionOption.fold("")(o => s" ($o)") + " · " +
      cpuModel.fold("")(o => s"$o ") + "(" + sys.runtime.availableProcessors + " threads)" +
      totalPhysicalMemory.fold("")(o => " " + toKiBGiB(o)) +
      s" · pid=${ProcessHandle.current.pid} " +
      (hostname.nonEmpty ?? s"host=$hostname ")
    ).trim

  def printlnWithClockIgnoringException(line: String): Unit =
    try printlnWithClock(line)
    catch { case _: Throwable => }

  def printlnWithClock(line: String): Unit =
    System.err.println(s"$nowString $line")

  def nowString: String =
    zonedDateTimeToString(ZonedDateTime.now)

  // Pattern like in log4j2.xml
  def zonedDateTimeToString(zonedDateTime: ZonedDateTime): String =
    zonedDateTime.toString
      .replace('T', ' ')
      .take(23)/*ms*/
      + zonedDateTime.getOffset.toString.replace(":", "")
