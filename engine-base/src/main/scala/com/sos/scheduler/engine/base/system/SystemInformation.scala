package com.sos.scheduler.engine.base.system

import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits._
import spray.json.DefaultJsonProtocol._

final case class SystemInformation(
  hostname: String,
  distribution: Option[String] = None,
  mxBeans: Map[String, Any] = Map())

object SystemInformation {
  implicit val MyJsonFormat = jsonFormat3(apply)

  val ForTest = SystemInformation(hostname = "HOSTNAME")
}
