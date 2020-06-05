package js7.common.system

import java.lang.management.ManagementFactory.{getOperatingSystemMXBean, getPlatformMBeanServer}
import javax.management.ObjectName
import js7.base.system.SystemInformation
import js7.common.system.OperatingSystem.operatingSystem
import scala.util.Try

object SystemInformations
{
  private def filteredMap(keyValues: Iterable[(String, Any)]): Map[String, Any] =
    (keyValues flatMap {
      case (_, v: Int) if v < 0 => Nil
      case o => o :: Nil
    }).toMap


  private def operatingSystemMXBean(): Map[String, Any] = {
    val bean = getOperatingSystemMXBean
    filteredMap(Map(
      "availableProcessors" -> bean.getAvailableProcessors,
      "systemLoadAverage" -> bean.getSystemLoadAverage))
  }

  private val OperatingSystemObjectName = new ObjectName("java.lang", "type", "OperatingSystem")
  private def platformMBean(): Map[String, Any] = {
    val bean = getPlatformMBeanServer
    val keys =
      "processCpuLoad" ::
      "systemCpuLoad" ::
      "totalPhysicalMemorySize" ::
      "committedVirtualMemorySize" ::
      "freePhysicalMemorySize" :: Nil
    filteredMap(for {
      key <- keys
      value <- Try { bean.getAttribute(OperatingSystemObjectName, key.capitalize) }.toOption
    } yield key -> value)
  }

  def systemInformation(): SystemInformation =
    SystemInformation(
      hostname = operatingSystem.hostname,
      distribution = operatingSystem.distributionNameAndVersionOption,
      cpuModel = operatingSystem.cpuModel,
      mxBeans = Map("operatingSystem" -> (
        operatingSystemMXBean() ++
        platformMBean())))
}
