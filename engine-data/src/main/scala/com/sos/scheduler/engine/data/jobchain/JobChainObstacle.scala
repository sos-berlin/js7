package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebased.{FileBasedObstacle, IsFileBasedObstacles}
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait JobChainObstacle

object JobChainObstacle {

  final case class FileBasedObstacles(fileBasedObstacles: Set[FileBasedObstacle])
  extends JobChainObstacle with IsFileBasedObstacles

  case object Stopped
  extends JobChainObstacle

  final case class OrderLimitReached(limit: Int)
  extends JobChainObstacle

  implicit val JobChainObstacleJsonFormat = TypedJsonFormat[JobChainObstacle](
    Subtype(jsonFormat1(FileBasedObstacles)),
    Subtype(jsonFormat0(() ⇒ Stopped)),
    Subtype(jsonFormat1(OrderLimitReached)))
}
