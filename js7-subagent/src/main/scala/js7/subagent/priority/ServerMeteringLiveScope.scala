package js7.subagent.priority

import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.problem.Checked
import js7.base.system.LiveBeanMapView
import js7.base.utils.KeyRenamingMapView
import js7.data.value.Value
import js7.data.value.expression.Scope
import scala.collection.MapView

/** Scope provides the current "live" server metering value. */
private[subagent] object ServerMeteringLiveScope extends Scope:

  override val nameToCheckedValue: MapView[String, Checked[Value]] =
    val rename = Seq(
      "cpuLoad" -> "js7CpuLoad",
      "committedVirtualMemorySize" -> "js7CommittedVirtualMemorySize",
      "freeMemorySize" -> "js7FreeMemorySize",
      "totalMemorySize" -> "js7TotalMemorySize")

    val bean = LiveBeanMapView(
      getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]),
      LiveBeanMapView.NameFilter(Set(
        "cpuLoad",
        "committedVirtualMemorySize",
        "freeMemorySize",
        "totalMemorySize")))

    KeyRenamingMapView(rename)(bean)
      .mapValues(Value.ofAny)

  override def toString =
    s"ServerMeteringLiveScope.Scope"
