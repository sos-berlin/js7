package com.sos.scheduler.engine.agent.views

import com.google.inject.ProvidedBy
import com.sos.scheduler.engine.agent.common.BeanPropertyReader
import com.sos.scheduler.engine.agent.common.BeanPropertyReader.ConditionalConverter
import com.sos.scheduler.engine.agent.views.AgentOverview._
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.common.sprayutils.SprayJson.implicits._
import com.sos.scheduler.engine.common.system.OperatingSystem
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
  currentProcessCount: Int,
  totalProcessCount: Int,
  isTerminating: Boolean,
  system: SystemInformation,
  java: JavaInformation)

object AgentOverview {
  final case class SystemInformation(
    hostname: String,
    mxBeans: Map[String, Any] = Map())

  object SystemInformation {
    implicit val MyJsonFormat = jsonFormat2(apply)

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
