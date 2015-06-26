package com.sos.scheduler.engine.agent.views

import com.google.inject.ProvidedBy
import com.sos.scheduler.engine.agent.views.AgentOverview._
import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.common.system.OperatingSystem
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
  final case class SystemInformation(hostname: String)

  object SystemInformation {
    lazy val Singleton = SystemInformation(
      hostname = OperatingSystem.operatingSystem.hostname)
    implicit val MyJsonFormat = jsonFormat1(apply)
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
