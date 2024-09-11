package js7.subagent.priority

import io.circe.syntax.EncoderOps
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.problem.Checked
import js7.base.system.LiveBeanMapView
import js7.base.test.OurTestSuite
import js7.base.time.Stopwatch
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.system.ServerMeteringEvent
import js7.data.value.{NumberValue, Value}
import scala.collection.MapView
import scala.math.Ordering.Implicits.infixOrderingOps

final class ServerMeteringLiveScopeTest extends OurTestSuite:

  "Fields" in:
    def getNumber(name: String): NumberValue =
      ServerMeteringLiveScope.nameToCheckedValue.checked(name).flatten.flatMap(_.toNumberValue)
        .orThrow

    assert(ServerMeteringLiveScope.nameToCheckedValue("js7CpuLoad").isRight) // May be MissingValue
    assert(getNumber("js7CommittedVirtualMemorySize") > NumberValue(0))
    assert(getNumber("js7FreeMemorySize") > NumberValue(0))
    assert(getNumber("js7TotalMemorySize") > NumberValue(0))

  "ServerMeteringLiveScope.fromEvent has same keys" in :
    assert:
      ServerMeteringEvent(None, 0, 0, 0)
        .toScope.nameToCheckedValue.keySet == ServerMeteringLiveScope.nameToCheckedValue.keySet

  "Speed test" in:
    val n = if isIntelliJIdea then 100_000 else 100

    Logger.info:
      val platformBean = LiveBeanMapView:
        getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
      .mapValues:
        Value.ofAny
      measureTime(n, "PlatformMxBeans", warmUp = n / 10):
        platformBean.toMap

    Logger.info:
      measureTime(n, "PriorityMxBeans", warmUp = n / 10):
        ServerMeteringLiveScope.nameToCheckedValue.toMap

    Logger.info:
      assert(ServerMeteringLiveScope.nameToCheckedValue.size == 4)
      measureTime(n, "Jsons", warmUp = n / 10):
        toJson(ServerMeteringLiveScope.nameToCheckedValue)
      + " · " + toJson(ServerMeteringLiveScope.nameToCheckedValue)

    speedTestSingleFieldBean("committedVirtualMemorySize")
    speedTestSingleFieldBean("freeMemorySize")
    speedTestSingleFieldBean("totalMemorySize")
    speedTestSingleFieldBean("cpuLoad") // Slower at a VM, 20x duration of the other fields

    def speedTestSingleFieldBean(name: String): Unit =
      Logger.info:
        val bean = LiveBeanMapView(
          getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]),
          LiveBeanMapView.NameFilter(Set(name)))
        assert(bean.toMap.size == 1)

        measureTime(n, name, warmUp = n / 10):
          bean.toMap
        + " · " + bean.toMap.toString

    def toJson(bean: MapView[String, Checked[Value]]): String =
      bean.collect:
        case (k, Right(v)) => k -> v
      .toMap.asJson.compactPrint
