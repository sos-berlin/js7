package com.sos.scheduler.engine.data.processclass

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebased.FileBasedObstacle
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait ProcessClassObstacle

object ProcessClassObstacle {

  final case class FileBasedObstacles(fileBasedObstacles: Set[FileBasedObstacle])
  extends ProcessClassObstacle

  final case class ProcessLimitReached(limit: Int)
  extends ProcessClassObstacle

  implicit val ProcessClassObstacleJsonFormat = TypedJsonFormat[ProcessClassObstacle](
    Subtype(jsonFormat1(FileBasedObstacles)),
    Subtype(jsonFormat1(ProcessLimitReached)))
}
