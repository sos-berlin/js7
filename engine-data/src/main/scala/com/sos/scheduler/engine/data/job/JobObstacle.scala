package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.filebased.FileBasedObstacle
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait JobObstacle

object JobObstacle {

  final case class FileBasedObstacles(fileBasedObstacles: Set[FileBasedObstacle])
  extends JobObstacle

  final case class BadState(jobState: JobState)
  extends JobObstacle

  final case class NoRuntime(plannedAt: Option[Instant])
  extends JobObstacle

  final case class TaskLimitReached(limit: Int)
  extends JobObstacle

  case object NonOrderJob
  extends JobObstacle

  implicit private val JobStateJsonFormat = JobState.MyJsonFormat
  implicit val JobObstacleJsonFormat = TypedJsonFormat[JobObstacle](
    Subtype(jsonFormat1(FileBasedObstacles)),
    Subtype(jsonFormat1(BadState)),
    Subtype(jsonFormat1(NoRuntime)),
    Subtype(jsonFormat1(TaskLimitReached)))
}
