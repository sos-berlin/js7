package com.sos.scheduler.engine.data.system

import spray.json.DefaultJsonProtocol._

final case class JavaInformation(systemProperties: Map[String, String])

object JavaInformation {
  private val JavaSystemPropertyKeys = List(
    "java.version",
    "java.vendor",
    "os.arch",
    "os.name",
    "os.version")
  val Singleton = new JavaInformation(systemProperties = (for (k ← JavaSystemPropertyKeys; v ← sys.props.get(k)) yield k → v).toMap)
  implicit val MyJsonFormat = jsonFormat1(apply)

  def apply(): JavaInformation = Singleton
}
