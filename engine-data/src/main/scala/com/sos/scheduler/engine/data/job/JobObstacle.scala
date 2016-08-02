package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.filebased.FileBasedObstacle
import java.time.Instant
import spray.json.DefaultJsonProtocol._
import spray.json._

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


  implicit object JobObstacleJsonFormat extends JsonFormat[JobObstacle] {
    implicit private val JobStateJsonFormat = JobState.MyJsonFormat
    implicit private val FileBasedObstaclesJsonFormat = jsonFormat1(FileBasedObstacles)
    implicit private val BadStateJsonFormat = jsonFormat1(BadState)
    implicit private val NoRuntimeJsonFormat = jsonFormat1(NoRuntime)
    implicit private val TaskLimitReachedJsonFormat = jsonFormat1(TaskLimitReached)
    private val typeFieldName = "type"
    private val FileBasedObstaclesName = JsString("FileBasedObstacles")
    private val BadStateName = JsString("BadState")
    private val NoRuntimeName = JsString("NoRuntime")
    private val TaskLimitReachedName = JsString("TaskLimit")

    def write(o: JobObstacle) = o match {
      case o: FileBasedObstacles ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → FileBasedObstaclesName))
      case o: BadState ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → BadStateName))
      case o: NoRuntime ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → NoRuntimeName))
      case o: TaskLimitReached ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → TaskLimitReachedName))
    }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields(typeFieldName) match {
        case FileBasedObstaclesName ⇒ json.convertTo[FileBasedObstacles]
        case BadStateName ⇒ json.convertTo[BadState]
        case NoRuntimeName ⇒ json.convertTo[NoRuntime]
        case TaskLimitReachedName ⇒ json.convertTo[TaskLimitReached]
      }
    }
  }
}
