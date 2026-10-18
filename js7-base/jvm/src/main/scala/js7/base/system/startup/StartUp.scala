package js7.base.system.startup

import java.io.File
import java.lang.management.ManagementFactory
import java.time.{ZoneId, ZonedDateTime}
import js7.base.BuildInfo
import js7.base.log.{CorrelId, Logger}
import js7.base.system.ServerOperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
import js7.base.system.SystemInformations.totalPhysicalMemory
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.Once
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
object StartUp:

  val startedAt: Timestamp =
    try Timestamp.ofEpochMilli(ManagementFactory.getRuntimeMXBean.getStartTime)
    catch case _: Throwable => Timestamp.now // paranoid

  private var _isMain = false
  private val logClasspathOnce = Once()

  def initializeMain(): Unit =
    _isMain = true

  def isMain: Boolean =
    _isMain

  def logJavaSettings(): Unit =
    // Do not initialize logging framework to early
    val logger = Logger[this.type]
    logger.whenDebugEnabled:
      logClasspathOnce: // Log only once (for tests running controller and agents in same JVM)
        logger.whenTraceEnabled:
          logger.debug("TRACE level logging is enabled")
        logCommandLine(logger)
        val paths = sys.props("java.class.path").nn.split(File.pathSeparator).filter(_.nonEmpty)
        logger.debug(s"Classpath contains ${paths.length} entries:")
        for o <- paths do
          logger.debug(s"Classpath $o")

        if CorrelId.isEnabled then
          // Check isEnabled after some debug line has been logged,
          // because CorrelIds may be enabled automatically on the first use by Log4j.
          CorrelId("CorrelId").bind:
            logger.debug("Correlation IDs are enabled")

  private def logCommandLine(logger: Logger): Unit =
    val info = ProcessHandle.current.info
    if info.command.isPresent && info.arguments.isPresent then
      // Linux and macOS
      logger.debug("Command line:")
      (info.command.get +: info.arguments.get).foreachWithBracket(): (arg: String, br) =>
        logger.debug(s"$br$arg")
    else
      // Windows
      // Limits of getInputArguments()
      // • It returns only the JVM arguments. It leaves out the main class or -jar file and the
      //   application arguments.
      // • It does not include options from JAVA_TOOL_OPTIONS, or does so inconsistently. The JVM
      //   prints "Picked up JAVA_TOOL_OPTIONS" to stderr instead.
      // • It does not expand @argfiles (a java @file argument) or JDK_JAVA_OPTIONS.
      // • It does not include the options that the java launcher itself consumes.
      logger.debug("Some Java options:") // Maybe incomplete
      ManagementFactory.getRuntimeMXBean.getInputArguments.asScala
        .foreachWithBracket(): (arg, br) =>
          logger.debug(s"$br$arg")

  def logStartUpLine(name: String): Unit =
    // Should be the very first log line after program start
    // The String "js7.base.system.startup - START " is expected
    Logger[this.type].info(s"START ${startUpLine(name)}")

  def startUpLine(name: String): String =
    s"""$name ${BuildInfo.prettyVersion} · $startUpLine
       |${"━" * 80}""".stripMargin // Log a bar, in case we append to an existing log file

  def logStopLine(): Unit =
    // Should be the very last log line before program exit
    // The String "js7.base.system.startup - STOP" is expected
    Logger[this.type].info("STOP")

  /** Log Java version, config and data directory, and classpath. */
  lazy val startUpLine: String =
    val zoneId = ZoneId.systemDefault
    "Java " + sys.props.getOrElse("java.version", "") + " · "
      + sys.props.getOrElse("java.vm.name", sys.props.getOrElse("java.runtime.name", "vm")) + " "
      + sys.props.getOrElse("java.vm.version", "")
      + " (" + toKiBGiB(sys.runtime.maxMemory) + ") · "
      + sys.props("os.name")
      + distributionNameAndVersionOption.fold("")(o => s" ($o)") + " · "
      + cpuModel.fold("")(o => s"$o ")
      + "(" + sys.runtime.availableProcessors + " threads)"
      + totalPhysicalMemory.fold("")(o => " " + toKiBGiB(o))
      + " · pid=" + ProcessHandle.current.pid
      + (hostname.nonEmpty ?? s" host=$hostname")
      // Show in the headline of each log file:
      + " · started at " + startedAt.toInstant.atZone(zoneId).withFixedOffsetZone
      + " " + zoneId // "Europe/Berlin"

  def printlnWithClockIgnoringException(line: String): Unit =
    try printlnWithClock(line)
    catch { case _: Throwable => }

  def printlnWithClock(line: String): Unit =
    System.err.println(s"$nowString $line")

  private def nowString: String =
    zonedDateTimeToString(ZonedDateTime.now)

  // Pattern like in log4j2.xml
  def zonedDateTimeToString(zonedDateTime: ZonedDateTime): String =
    zonedDateTime.toString
      .replace('T', ' ')
      .take(23)/*ms*/
      + zonedDateTime.getOffset.toString.replace(":", "")

  private val sinceNano = System.nanoTime()

  def elapsed: FiniteDuration =
    elapsedNanos.ns

  def elapsedNanos: Long =
    System.nanoTime() - sinceNano
