package js7.subagent.priority

import com.sun.management.OperatingSystemMXBean
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.problem.Checked
import js7.base.system.LiveBeanMapView
import js7.data.value.Value
import js7.data.value.expression.Scope

/** Scope provides the current "live" server metering value. */
private[subagent] object ServerMeteringLiveScope extends Scope:

  val bean: LiveBeanMapView[OperatingSystemMXBean] =
    LiveBeanMapView(
      getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]),
      LiveBeanMapView.NameFilter(Set(
        "cpuLoad",
        "committedVirtualMemorySize",
        "freeMemorySize",
        "totalMemorySize")))

  override def namedValue(name: String): Option[Checked[Value]] =
    name.match
      case "js7CpuLoad" => bean.get("cpuLoad")
      case "js7CommittedVirtualMemorySize" => bean.get("committedVirtualMemorySize")
      case "js7FreeMemorySize" => bean.get("freeMemorySize")
      case "js7TotalMemorySize" => bean.get("totalMemorySize")
    .map(Value.ofAny)

  override def toString =
    s"ServerMeteringLiveScope.Scope"
