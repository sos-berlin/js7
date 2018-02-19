package com.sos.jobscheduler.base.system

import com.sun.management.HotSpotDiagnosticMXBean
import java.lang.management.ManagementFactory.{getPlatformMBeanServer, newPlatformMXBeanProxy}
import java.nio.file.Path

object JavaHeapDump
{
  private lazy val hotspotMBean = newPlatformMXBeanProxy(
    getPlatformMBeanServer,
    "com.sun.management:type=HotSpotDiagnostic",
    classOf[HotSpotDiagnosticMXBean])

  def dumpHeapTo(file: Path): Unit = {
    hotspotMBean.dumpHeap(file.toString, /*liveObjectsOnly*/true)
    System.err.println(s"Java Heap dump written to ${file.toAbsolutePath}")
  }
}
