package com.sos.scheduler.engine.data.common

import spray.json.DefaultJsonProtocol._
import spray.json.{JsValue, RootJsonReader}

/**
  * @author Joacim Zschimmer
  */
trait WebError {
  def message: String
}

object WebError {
  def apply(message: String) = Simple(message)

  final case class Simple(message: String) extends WebError

  implicit val SimpleJsonFormat = jsonFormat1(Simple.apply)

  implicit val jsonFormat = new RootJsonReader[WebError] {
    override def read(json: JsValue) = json.convertTo[Simple]
  }
}
