package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.common.Obstacle
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedObstacle
extends Obstacle

object FileBasedObstacle {

  case object Missing
  extends FileBasedObstacle

  final case class BadState(State: FileBasedState, message: Option[String])
  extends FileBasedObstacle

  //case object ToBeChanged ?
  //case object ToBeRemoved ?

  implicit object FileBasedJsonFormat extends JsonFormat[FileBasedObstacle] {
    private val typeFieldName = "type"
    implicit private val FileBasedStateJsonFormat = FileBasedState.MyJsonFormat
    implicit private val MissingJsonFormat = jsonFormat0(() ⇒ Missing)
    implicit private val BadStateJsonFormat = jsonFormat2(BadState)
    private val MissingName = JsString("Missing")
    private val BadStateName = JsString("BadState")

    def write(o: FileBasedObstacle) = o match {
      case Missing ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → MissingName))
      case o: BadState ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → BadStateName))
    }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields(typeFieldName) match {
        case MissingName ⇒ json.convertTo[Missing.type]
        case BadStateName ⇒ json.convertTo[BadState]
      }
    }
  }
}
