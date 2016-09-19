package com.sos.scheduler.engine.data.lock

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebased.FileBasedObstacle
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
// NOT YET USED
private sealed trait LockObstacle

private object LockObstacle {
  final case class FileBasedObstacles(fileBasedObstacles: Set[FileBasedObstacle])
  extends LockObstacle

  case object LockedExclusivly
  extends LockObstacle

  final case class LockedNonExclusivly(limit: Int)
  extends LockObstacle

  implicit val LockObstacleJsonFormat = TypedJsonFormat[LockObstacle](
    Subtype(jsonFormat1(FileBasedObstacles)),
    Subtype(jsonFormat0(() ⇒ LockedExclusivly)),
    Subtype(jsonFormat1(LockedNonExclusivly)))
}
