package js7.subagent.priority

import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.system.LiveBeanMapView
import js7.data.value.Value
import js7.data.value.expression.Scope

/** Scope provides the current "live" server metering value. */
private[subagent] object ServerMeteringLiveScope extends Scope:

  override val nameToCheckedValue =
    LiveBeanMapView(
      getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]),
      LiveBeanMapView.NameFilter(Set(
        "cpuLoad",
        "committedVirtualMemorySize",
        "freeMemorySize",
        "totalMemorySize"))
    ).mapValues:
      Value.ofAny

  override def toString =
    s"ServerMeteringLiveScope.Scope"
