package js7.data.system

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StandardMapView
import js7.data.event.{NoKeyEvent, NonPersistentEvent}
import js7.data.value.expression.Scope
import js7.data.value.{MissingValue, NumberValue, Value}

final case class ServerMeteringEvent(
  cpuLoad: Option[Double],
  committedVirtualMemorySize: Long,
  freeMemorySize: Long,
  totalMemorySize: Long,
  testMeteringValue: Option[Double] = None)
extends NoKeyEvent, NonPersistentEvent:

  def toScope: Scope =
    new Scope:
      override val nameToCheckedValue =
        new StandardMapView[String, Checked[Value]]:
          def get(key: String) =
            key match
              case "cpuLoad" =>
                Some(Right(cpuLoad.fold_(MissingValue, NumberValue(_))))
              case "committedVirtualMemorySize" =>
                Some(Right(NumberValue(committedVirtualMemorySize)))
              case "freeMemorySize" =>
                Some(Right(NumberValue(freeMemorySize)))
              case "totalMemorySize" =>
                Some(Right(NumberValue(totalMemorySize)))
              case "testMeteringValue" =>
                testMeteringValue.map(o => Right(NumberValue(o)))
              case _ =>
                None

          override val keySet =
            Set("cpuLoad", "committedVirtualMemorySize", "freeMemorySize", "totalMemorySize") ++
              testMeteringValue.map(_ => "testMeteringValue").toSet

//def toScope: Scope =
  //  var map = Map(
  //    "cpuLoad" -> cpuLoad.fold_(MissingValue, NumberValue(_)),
  //    "committedVirtualMemorySize" -> NumberValue(committedVirtualMemorySize),
  //    "freeMemorySize" -> NumberValue(freeMemorySize),
  //    "totalMemorySize" -> NumberValue(totalMemorySize))
  //
  //  for o <- testMeteringValue do
  //    map = map.updated("testMeteringValue", NumberValue(o))
  //
  //  new NamedValueScope(map.view.mapValues(Right(_)))


object ServerMeteringEvent:

  def fromCurrentMxBean(): ServerMeteringEvent =
    val bean = getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
    ServerMeteringEvent(
      cpuLoad =
        val o = bean.getCpuLoad
        !o.isNaN ? o,
      committedVirtualMemorySize = bean.getCommittedVirtualMemorySize,
      freeMemorySize = bean.getFreeMemorySize,
      totalMemorySize = bean.getTotalMemorySize)

  given Codec.AsObject[ServerMeteringEvent] = deriveCodec[ServerMeteringEvent]
