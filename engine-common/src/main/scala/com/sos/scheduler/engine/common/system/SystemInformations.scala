package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.base.system.SystemInformation
import com.sos.scheduler.engine.common.utils.BeanPropertyReader
import com.sos.scheduler.engine.common.utils.BeanPropertyReader.ConditionalConverter
import java.lang.management.ManagementFactory.getOperatingSystemMXBean
import java.lang.management.OperatingSystemMXBean

object SystemInformations {
  private val OnlyNonNegative: ConditionalConverter = { case v: Number if v.doubleValue >= 0 ⇒ v }

  private val operatingSystemMXBeanReader = new BeanPropertyReader[OperatingSystemMXBean](getOperatingSystemMXBean.getClass, {
    case "availableProcessors" ⇒ OnlyNonNegative
    case "systemCpuLoad" ⇒ OnlyNonNegative
    case "processCpuLoad" ⇒ OnlyNonNegative
    case "committedVirtualMemorySize" ⇒ OnlyNonNegative
    case "totalPhysicalMemorySize" ⇒ OnlyNonNegative
    case "freePhysicalMemorySize" ⇒ OnlyNonNegative
  })

  def systemInformation(): SystemInformation = {
    import OperatingSystem.operatingSystem.{cpuModel, distributionNameAndVersionOption, hostname}
    SystemInformation(
      hostname = hostname,
      distribution = distributionNameAndVersionOption,
      cpuModel = cpuModel,
      mxBeans = Map("operatingSystem" → operatingSystemMXBeanReader.toMap(getOperatingSystemMXBean)))
  }
}
