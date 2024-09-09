package js7.subagent.priority

import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.system.BeanMapView
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.system.SubagentPriorityDataEvent
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NamedValueScope
import js7.data.value.{MissingValue, NumberValue, Value}

private[subagent] object PriorityMxBeanScope extends Scope:

  override val nameToCheckedValue =
    BeanMapView(
      getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]),
      BeanMapView.NameFilter(Set(
        "cpuLoad",
        "committedVirtualMemorySize",
        "freeMemorySize",
        "totalMemorySize"))
    ).mapValues:
      Value.ofAny

  override def toString =
    s"PriorityMxBeanScope.Scope"


object SubagentPriorityDataEventScope:
  def apply(event: SubagentPriorityDataEvent): Scope =
    NamedValueScope(
      "cpuLoad" -> event.cpuLoad.fold_(MissingValue, NumberValue(_)),
      "committedVirtualMemorySize" -> NumberValue(event.committedVirtualMemorySize),
      "freeMemorySize" -> NumberValue(event.freeMemorySize),
      "totalMemorySize" -> NumberValue(event.totalMemorySize))
