package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.filebased.FileBasedObstacle
import java.time.Instant
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderObstacle

object OrderObstacle {

  final case class FileBasedObstacles(fileBasedObstacles: Set[FileBasedObstacle])
  extends OrderObstacle

  case object Suspended
  extends OrderObstacle

  case object Blacklisted
  extends OrderObstacle

  final case class Setback(until: Instant)
  extends OrderObstacle

  implicit val MyJsonFormat = new RootJsonFormat[OrderObstacle] {
    implicit private val FileBasedObstaclesJsonFormat = jsonFormat1(FileBasedObstacles)
    implicit private val SetbackJsonFormat = jsonFormat1(Setback)
    private val typeFieldName = "type"
    private val FileBasedObstaclesName = JsString("FileBasedObstacles")
    private val SuspendedName = JsString("Suspended")
    private val BlacklistedName = JsString("Blacklisted")
    private val SetbackName = JsString("Setback")

    def write(o: OrderObstacle) =
      o match {
        case o: FileBasedObstacles ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → FileBasedObstaclesName))
        case Suspended ⇒ JsObject(typeFieldName → SuspendedName)
        case Blacklisted ⇒ JsObject(typeFieldName → BlacklistedName)
        case o: Setback ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → SetbackName))
      }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields(typeFieldName) match {
        case FileBasedObstaclesName ⇒ json.convertTo[FileBasedObstacles]
        case SuspendedName ⇒ Suspended
        case BlacklistedName ⇒ Blacklisted
        case SetbackName ⇒ json.convertTo[Setback]
      }
    }
  }
}
