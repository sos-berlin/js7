package js7.base.system

import java.lang.management.ManagementFactory
import js7.base.system.ServerOperatingSystem.operatingSystem

object SystemInformations:

  private lazy val operatingSystemMXBean = ManagementFactory.getPlatformMXBean:
    classOf[com.sun.management.OperatingSystemMXBean]

  def totalPhysicalMemory: Option[Long] =
    Some(operatingSystemMXBean.getTotalMemorySize)

  def systemInformation(): SystemInformation =
    SystemInformation(
      hostname = operatingSystem.hostname,
      distribution = operatingSystem.distributionNameAndVersionOption,
      cpuModel = operatingSystem.cpuModel,
      mxBeans = Map(
        "operatingSystem" -> LiveBeanMapView(operatingSystemMXBean).toMap))
