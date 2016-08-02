package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import java.time.Instant
import spray.json._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderObstacle

object OrderObstacle {

  case object Suspended
  extends OrderObstacle

  case object Blacklisted
  extends OrderObstacle

  final case class Setback(until: Instant)
  extends OrderObstacle

  implicit val MyJsonFormat = new RootJsonFormat[OrderObstacle] {
    private val typeFieldName = "type"
    private val SuspendedName = JsString("Suspended")
    private val BlacklistedName = JsString("Blacklisted")
    private val SetbackName = JsString("Setback")

    def write(o: OrderObstacle) =
      o match {
        case Suspended ⇒ JsObject(typeFieldName → SuspendedName)
        case Blacklisted ⇒ JsObject(typeFieldName → BlacklistedName)
        case Setback(until) ⇒ JsObject(typeFieldName → SetbackName, "until" → until.toJson)
      }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields(typeFieldName) match {
        case SuspendedName ⇒ Suspended
        case BlacklistedName ⇒ Blacklisted
        case SetbackName ⇒ Setback(fields("until").convertTo[Instant])
      }
    }
  }
}
