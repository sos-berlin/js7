package com.sos.jobscheduler.base.sprayjson

import com.sos.jobscheduler.base.utils.ScalaUtils._
import io.circe._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
object CirceToSpray {

  def circeToSpray(json: Json): JsValue =
    json.asBoolean match {
      case Some(o) ⇒
        JsBoolean(o)
      case None ⇒
        json.asNumber match {
          case Some(o) ⇒
            JsNumber(o.toBigDecimal.get)
          case None ⇒
            json.asString match {
              case Some(o) ⇒
                JsString(o)
              case None if json.isNull ⇒
                JsNull
              case None ⇒
                json.asObject match {
                  case Some(o) ⇒
                    JsObject(o.toMap filter (!_._2.isNull) mapValues circeToSpray)
                  case None ⇒
                    json.asArray match {
                      case Some(o) ⇒ JsArray(o map circeToSpray)
                      case None ⇒ throw new RuntimeException(s"Unsupported Circe JSON type: ${json.getClass.simpleName}")
                    }
                }
            }
        }
    }
}
