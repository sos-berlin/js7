package js7.data.system

import io.circe.Codec
import io.circe.generic.semiauto
import io.circe.generic.semiauto.deriveCodec
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.event.{NoKeyEvent, NonPersistentEvent}

final case class SubagentPriorityDataEvent(
  cpuLoad: Option[Double],
  committedVirtualMemorySize: Long,
  freeMemorySize: Long,
  totalMemorySize: Long,
  testPriority: Option[Double] = None)
extends NoKeyEvent, NonPersistentEvent

object SubagentPriorityDataEvent:

  def fromCurrentMxBean(): SubagentPriorityDataEvent =
    val bean = getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
    SubagentPriorityDataEvent(
      cpuLoad =
        val o = bean.getCpuLoad
        !o.isNaN ? o,
      committedVirtualMemorySize = bean.getCommittedVirtualMemorySize,
      freeMemorySize = bean.getFreeMemorySize,
      totalMemorySize = bean.getTotalMemorySize)

  given Codec.AsObject[SubagentPriorityDataEvent] = deriveCodec[SubagentPriorityDataEvent]
