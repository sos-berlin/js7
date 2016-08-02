package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.job.TaskId
import java.time.Instant
import spray.json._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderProcessingState

object OrderProcessingState {

  case object NotPlanned
  extends OrderProcessingState

  final case class Planned(at: Instant)
  extends OrderProcessingState

  final case class Late(at: Instant)
  extends OrderProcessingState

  sealed trait InTask
  extends OrderProcessingState {
    def taskId: TaskId
  }

  sealed trait Waiting
  extends OrderProcessingState

  final case class WaitingInTask(taskId: TaskId)
  extends InTask with Waiting

  final case class InTaskProcess(taskId: TaskId)
  extends InTask

  final case class Setback(until: Instant)
  extends Waiting

  case object Blacklisted
  extends OrderProcessingState

  case object Suspended extends Waiting

  case object WaitingForOther extends Waiting

  implicit val MyJsonFormat = new RootJsonFormat[OrderProcessingState] {
    private val typeFieldName = "type"
    private val NotPlannedName = JsString("NotPlanned")
    private val PlannedName = JsString("Planned")
    private val LateName = JsString("Late")
    private val InTaskWaitingName = JsString("WaitingInTask")
    private val InTaskProcessName = JsString("InTaskProcess")
    private val SetbackName = JsString("Setback")
    private val BlacklistedName = JsString("Blacklisted")
    private val SuspendedName = JsString("Suspended")
    private val DelayedForOtherName = JsString("WaitingForOther")

    def write(o: OrderProcessingState) =
      o match {
        case NotPlanned ⇒ JsObject(typeFieldName → NotPlannedName)
        case Planned(at) ⇒ JsObject(typeFieldName → PlannedName, "at" → at.toJson)
        case Late(at) ⇒ JsObject(typeFieldName → LateName, "at" → at.toJson)
        case WaitingInTask(taskId) ⇒ JsObject(typeFieldName → InTaskWaitingName, "taskId" → taskId.toJson)
        case InTaskProcess(taskId) ⇒ JsObject(typeFieldName → InTaskProcessName, "taskId" → taskId.toJson)
        case Blacklisted ⇒ JsObject(typeFieldName → BlacklistedName)
        case Setback(at) ⇒ JsObject(typeFieldName → SetbackName, "until" → at.toJson)
        case Suspended ⇒ JsObject(typeFieldName → SuspendedName)
        case WaitingForOther ⇒ JsObject(typeFieldName → DelayedForOtherName)
      }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields(typeFieldName) match {
        case NotPlannedName ⇒ NotPlanned
        case PlannedName ⇒ Planned(fields("at").convertTo[Instant])
        case LateName ⇒ Late(fields("at").convertTo[Instant])
        case InTaskWaitingName ⇒ WaitingInTask(fields("taskId").convertTo[TaskId])
        case InTaskProcessName ⇒ InTaskProcess(fields("taskId").convertTo[TaskId])
        case SetbackName ⇒ Setback(fields("until").convertTo[Instant])
        case BlacklistedName ⇒ Blacklisted
        case SuspendedName ⇒ Suspended
        case DelayedForOtherName ⇒ WaitingForOther
        case JsString(o) ⇒ throw new IllegalArgumentException(s"""Unknown value for field OrderProcessingState."type"=$o""")
        case _ ⇒ throw new IllegalArgumentException("""Unknown value for field OrderProcessingState."type"""")
      }
    }
  }
}
