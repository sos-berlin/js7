package js7.subagent.priority

import io.circe.syntax.EncoderOps
import java.lang.management.ManagementFactory.getPlatformMXBean
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.problem.Problem
import js7.base.system.LiveBeanMapView
import js7.base.test.OurTestSuite
import js7.base.time.Stopwatch
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.value.{NumberValue, Value}
import scala.collection.MapView
import scala.math.Ordering.Implicits.infixOrderingOps

final class ServerMeteringLiveScopeTest extends OurTestSuite:
  private val logger = Logger[this.type]

  "Fields" in:
    def getNumber(name: String): NumberValue =
      ServerMeteringLiveScope.namedValue(name)
        .toRight(Problem(s"Missing: $name")).flatten
        .flatMap(_.toNumberValue)
        .orThrow

    assert(ServerMeteringLiveScope.namedValue("js7CpuLoad").exists(_.isRight)) // May be MissingValue
    assert(getNumber("js7CommittedVirtualMemorySize") > NumberValue(0))
    assert(getNumber("js7FreeMemorySize") > NumberValue(0))
    assert(getNumber("js7TotalMemorySize") > NumberValue(0))

  "Speed test" in:
    val n = if isIntelliJIdea then 100_000 else 100

    logger.info:
      val platformBean = LiveBeanMapView:
        getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean])
      .mapValues:
        Value.ofAny
      measureTime(n, "PlatformMxBeans", warmUp = n / 10):
        platformBean.toMap

    logger.info:
      measureTime(n, "PriorityMxBeans", warmUp = n / 10):
        ServerMeteringLiveScope.namedValue("js7CpuLoad")
        ServerMeteringLiveScope.namedValue("js7CommittedVirtualMemorySize")
        ServerMeteringLiveScope.namedValue("js7FreeMemorySize")
        ServerMeteringLiveScope.namedValue("js7TotalMemorySize")

    assert(ServerMeteringLiveScope.bean.size == 4)
    logger.info:
      measureTime(n, "Jsons", warmUp = n / 10):
        toJson(ServerMeteringLiveScope.bean)
          + " · " + toJson(ServerMeteringLiveScope.bean)

    speedTestSingleFieldBean("committedVirtualMemorySize")
    speedTestSingleFieldBean("freeMemorySize")
    speedTestSingleFieldBean("totalMemorySize")
    speedTestSingleFieldBean("cpuLoad") // Slower at a VM, 20x duration of the other fields

    def speedTestSingleFieldBean(name: String): Unit =
      val bean = LiveBeanMapView(
        getPlatformMXBean(classOf[com.sun.management.OperatingSystemMXBean]),
        LiveBeanMapView.NameFilter(Set(name)))
      assert(bean.toMap.size == 1)
      logger.info:
        measureTime(n, name, warmUp = n / 10):
          bean.toMap
        + " · " + bean.toMap.toString

    def toJson(bean: MapView[String, Any]): String =
      bean.mapValues(Value.ofAny).collect:
        case (k, Right(v)) => k -> v
      .toMap.asJson.compactPrint
