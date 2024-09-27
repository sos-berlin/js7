package js7.data.system

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StandardMapView
import js7.data.event.{NoKeyEvent, NonPersistentEvent}
import js7.data.system.ServerMeteringEvent.*
import js7.data.value.expression.Scope
import js7.data.value.{MissingValue, NumberValue, Value}
import scala.collection.MapView

final case class ServerMeteringEvent(
  cpuLoad: Option[Double],
  committedVirtualMemorySize: Long,
  freeMemorySize: Long,
  totalMemorySize: Long,
  testMeteringValue: Option[Double] = None)
extends NoKeyEvent, NonPersistentEvent:
  // testMeteringValue is used only for build tests.

  def toScope: Scope =
    new Scope:
      override val nameToCheckedValue: MapView[String, Checked[Value]] =
        new StandardMapView[String, Checked[Value]]:
          def get(key: String) =
            key match
              case "js7CpuLoad" =>
                Some(Right(cpuLoad.filter(_ >= 0.0).fold_(MissingValue, NumberValue(_))))
              case "js7CommittedVirtualMemorySize" =>
                someValue(committedVirtualMemorySize)
              case "js7FreeMemorySize" =>
                someValue(freeMemorySize)
              case "js7TotalMemorySize" =>
                someValue(totalMemorySize)
              case "js7TestMeteringValue" =>
                testMeteringValue.map(o => Right(NumberValue(o)))
              case _ =>
                None

          override lazy val keySet =
            Set("js7CpuLoad",
              "js7CommittedVirtualMemorySize", "js7FreeMemorySize", "js7TotalMemorySize"
            ) ++ testMeteringValue.map(_ => "js7TestMeteringValue")


object ServerMeteringEvent:

  def fromCurrentMxBean(): ServerMeteringEvent =
    val bean = getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
    ServerMeteringEvent(
      cpuLoad = normalizePositive(bean.getCpuLoad),
      committedVirtualMemorySize = bean.getCommittedVirtualMemorySize,
      freeMemorySize = bean.getFreeMemorySize,
      totalMemorySize = bean.getTotalMemorySize)

  private def normalizePositive(double: Double): Option[Double] =
    (!double.isNaN && double >= 0) ? double

  given Codec.AsObject[ServerMeteringEvent] = deriveCodec[ServerMeteringEvent]

  private def someValue(long: Long) = Some(Right(NumberValue(long)))
