package js7.base.system

import com.sun.management.HotSpotDiagnosticMXBean
import java.lang.management.ManagementFactory.{getPlatformMBeanServer, newPlatformMXBeanProxy}
import java.nio.file.Files.deleteIfExists
import java.nio.file.Path
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.Deadline.now

object JavaHeapDump:
  private val logger = Logger[this.type]

  private lazy val hotspotMBean = newPlatformMXBeanProxy(
    getPlatformMBeanServer,
    "com.sun.management:type=HotSpotDiagnostic",
    classOf[HotSpotDiagnosticMXBean])

  def dumpHeapTo(file: Path, overwrite: Boolean = false): Unit =
    logger.debugCall:
      val since = now
      if overwrite then
        deleteIfExists(file)
      hotspotMBean.dumpHeap(file.toString, true)
      val msg = s"Java Heap dump written to ${file.toAbsolutePath} (${since.elapsed.pretty})"
      System.err.println(msg)
      logger.info(msg)
