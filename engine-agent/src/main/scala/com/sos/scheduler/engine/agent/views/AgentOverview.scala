package com.sos.scheduler.engine.agent.views

import com.google.inject.ProvidedBy
import com.sos.scheduler.engine.agent.views.AgentOverview._
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.SprayJson
import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits._
import com.sos.scheduler.engine.common.system.OperatingSystem
import com.sos.scheduler.engine.common.utils.BeanPropertyReader
import com.sos.scheduler.engine.common.utils.BeanPropertyReader.ConditionalConverter
import java.lang.management.ManagementFactory.getOperatingSystemMXBean
import java.lang.management.OperatingSystemMXBean
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
 * @author Joacim Zschimmer
 */
@ProvidedBy(classOf[AgentOverviewProvider])
final case class AgentOverview(
  version: String,
  startedAt: Instant,
  currentTaskCount: Int,
  totalTaskCount: Int,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview {
  final case class SystemInformation(
    hostname: String,
    distribution: Option[String] = None,
    mxBeans: Map[String, Any] = Map())

  object SystemInformation {
    implicit val MyJsonFormat = jsonFormat3(apply)

    private val OnlyNonNegative: ConditionalConverter = { case v: Number if v.doubleValue >= 0 ⇒ v }

    private val operatingSystemMXBeanReader = new BeanPropertyReader[OperatingSystemMXBean](getOperatingSystemMXBean.getClass, {
      case "availableProcessors" ⇒ OnlyNonNegative
      case "systemCpuLoad" ⇒ OnlyNonNegative
      case "processCpuLoad" ⇒ OnlyNonNegative
      case "committedVirtualMemorySize" ⇒ OnlyNonNegative
      case "totalPhysicalMemorySize" ⇒ OnlyNonNegative
      case "freePhysicalMemorySize" ⇒ OnlyNonNegative
    })

    def apply(): SystemInformation = SystemInformation(
      hostname = OperatingSystem.operatingSystem.hostname,
      distribution = OperatingSystem.operatingSystem.distributionNameAndVersionOption,
      mxBeans = Map("operatingSystem" → operatingSystemMXBeanReader.toMap(getOperatingSystemMXBean)))
  }

  final case class JavaInformation(systemProperties: Map[String, String])

  object JavaInformation {
    private val JavaSystemPropertyKeys = List(
      "java.version",
      "java.vendor",
      "os.arch",
      "os.name",
      "os.version")
    val Singleton = JavaInformation(systemProperties = (for (k ← JavaSystemPropertyKeys; v ← sys.props.get(k)) yield k → v).toMap)
    implicit val MyJsonFormat = jsonFormat1(apply)
  }

  implicit val MyJsonFormat = jsonFormat7(apply)
}
