package com.sos.jobscheduler.common.system

import com.sos.jobscheduler.base.system.SystemInformation
import java.lang.management.ManagementFactory.{getOperatingSystemMXBean, getPlatformMBeanServer}
import javax.management.ObjectName
import scala.util.Try

object SystemInformations {
  private def filteredMap(keyValues: Iterable[(String, Any)]): Map[String, Any] =
    (keyValues flatMap {
      case (_, v: Int) if v < 0 ⇒ None
      case o ⇒ Some(o)
    }).toMap


  private def operatingSystemMXBean(): Map[String, Any] = {
    val bean = getOperatingSystemMXBean
    filteredMap(Map(
      "availableProcessors" → bean.getAvailableProcessors,
      "systemLoadAverage" → bean.getSystemLoadAverage))
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
      key ← keys
      value ← Try { bean.getAttribute(OperatingSystemObjectName, key.capitalize) }.toOption
    } yield key → value)
  }

  def systemInformation(): SystemInformation = {
    import OperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
    SystemInformation(
      hostname = hostname,
      distribution = distributionNameAndVersionOption,
      cpuModel = cpuModel,
      mxBeans = Map("operatingSystem" → (
        operatingSystemMXBean() ++
        platformMBean())))
  }
}
