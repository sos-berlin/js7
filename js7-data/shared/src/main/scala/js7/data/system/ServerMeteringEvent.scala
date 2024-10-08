package js7.data.system

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.metering.CallMeter
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{NoKeyEvent, NonPersistentEvent}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.NamedValueScope
import js7.data.value.{MissingValue, NumberValue, Value}

/**
 * @param testMeteringValue is used only for build tests.
 */
final case class ServerMeteringEvent(
  cpuLoad: Option[Double],
  committedVirtualMemorySize: Long,
  freeMemorySize: Long,
  totalMemorySize: Long,
  testMeteringValue: Option[Double] = None)
extends NoKeyEvent, NonPersistentEvent:

  def toScope: Scope =
    NamedValueScope.simple:
      case "js7CpuLoad" => cpuLoad.filter(_ >= 0.0).fold_(MissingValue, NumberValue(_))
      case "js7CommittedVirtualMemorySize" => NumberValue(committedVirtualMemorySize)
      case "js7FreeMemorySize" => NumberValue(freeMemorySize)
      case "js7TotalMemorySize" => NumberValue(totalMemorySize)
      case "js7TestMeteringValue" if testMeteringValue.isDefined =>
        NumberValue(testMeteringValue.get)


object ServerMeteringEvent:
  private val meter = CallMeter("ServerMeteringEvent.fromCurrentMxBean")

  def fromCurrentMxBean(): ServerMeteringEvent =
    meter:
      val bean = getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
      ServerMeteringEvent(
        cpuLoad = normalizePositive(bean.getCpuLoad),
        committedVirtualMemorySize = bean.getCommittedVirtualMemorySize,
        freeMemorySize = bean.getFreeMemorySize,
        totalMemorySize = bean.getTotalMemorySize)

  private def normalizePositive(double: Double): Option[Double] =
    (!double.isNaN && double >= 0) ? double

  given Codec.AsObject[ServerMeteringEvent] = deriveCodec[ServerMeteringEvent]
