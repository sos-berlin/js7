package com.sos.scheduler.engine.base.system

import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import spray.json.DefaultJsonProtocol._

final case class SystemInformation(
  hostname: String,
  distribution: Option[String] = None,
  cpuModel: Option[String] = None,
  mxBeans: Map[String, Any] = Map())

object SystemInformation {
  implicit val MyJsonFormat = jsonFormat4(apply)

  val ForTest = SystemInformation(hostname = "HOSTNAME")
}
