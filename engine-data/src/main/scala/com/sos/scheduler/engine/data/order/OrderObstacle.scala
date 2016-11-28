package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebased.{FileBasedObstacle, IsFileBasedObstacles}
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderObstacle

object OrderObstacle {

  final case class FileBasedObstacles(fileBasedObstacles: Set[FileBasedObstacle])
  extends OrderObstacle with IsFileBasedObstacles

  case object Suspended
  extends OrderObstacle

  case object Blacklisted
  extends OrderObstacle

  final case class Setback(until: Instant)
  extends OrderObstacle

  implicit val MyJsonFormat = TypedJsonFormat[OrderObstacle](
    Subtype(jsonFormat1(FileBasedObstacles)),
    Subtype(jsonFormat0(() ⇒ Suspended)),
    Subtype(jsonFormat0(() ⇒ Blacklisted)),
    Subtype(jsonFormat1(Setback)))
}
