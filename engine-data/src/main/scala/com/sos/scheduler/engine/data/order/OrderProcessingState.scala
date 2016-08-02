package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.data.job.TaskId
import java.time.Instant
import spray.json.DefaultJsonProtocol._
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

  final case class Pending(at: Instant)
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


  implicit object OrderProcessingStateJsonFormat extends RootJsonFormat[OrderProcessingState] {
    private implicit val NotPlannedJsonFormat = jsonFormat0(() ⇒ NotPlanned)
    private implicit val PlannedJsonFormat = jsonFormat1(Planned)
    private implicit val PendingJsonFormat = jsonFormat1(Pending)
    private implicit val WaitingInTaskJsonFormat = jsonFormat1(WaitingInTask)
    private implicit val InTaskProcessJsonFormat = jsonFormat1(InTaskProcess)
    private implicit val SetbackJsonFormat = jsonFormat1(Setback)
    private implicit val BlacklistedJsonFormat = jsonFormat0(() ⇒ Blacklisted)
    private implicit val SuspendedJsonFormat = jsonFormat0(() ⇒ Suspended)
    private implicit val WaitingForOtherJsonFormat = jsonFormat0(() ⇒ WaitingForOther)
    private val typeFieldName = "type"
    private val NotPlannedName = JsString("NotPlanned")
    private val PlannedName = JsString("Planned")
    private val PendingName = JsString("Pending")
    private val WaitingInTaskName = JsString("WaitingInTask")
    private val InTaskProcessName = JsString("InTaskProcess")
    private val SetbackName = JsString("Setback")
    private val BlacklistedName = JsString("Blacklisted")
    private val SuspendedName = JsString("Suspended")
    private val WaitingForOtherName = JsString("WaitingForOther")

    def write(o: OrderProcessingState) = o match {
      case NotPlanned ⇒ JsObject(typeFieldName → NotPlannedName)
      case o: Planned ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → PlannedName))
      case o: Pending ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → PendingName))
      case o: WaitingInTask ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → WaitingInTaskName))
      case o: InTaskProcess ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → InTaskProcessName))
      case Blacklisted ⇒ JsObject(typeFieldName → BlacklistedName)
      case o: Setback ⇒ JsObject(o.toJson.asJsObject.fields + (typeFieldName → SetbackName))
      case Suspended ⇒ JsObject(typeFieldName → SuspendedName)
      case WaitingForOther ⇒ JsObject(typeFieldName → WaitingForOtherName)
    }

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      fields(typeFieldName) match {
        case NotPlannedName ⇒ NotPlanned
        case PlannedName ⇒ json.convertTo[Planned]
        case PendingName ⇒ json.convertTo[Pending]
        case WaitingInTaskName ⇒ json.convertTo[WaitingInTask]
        case InTaskProcessName ⇒ json.convertTo[InTaskProcess]
        case SetbackName ⇒ json.convertTo[Setback]
        case BlacklistedName ⇒ Blacklisted
        case SuspendedName ⇒ Suspended
        case WaitingForOtherName ⇒ WaitingForOther
        case JsString(o) ⇒ throw new IllegalArgumentException(s"""Unknown value for field OrderProcessingState."type"=$o""")
        case _ ⇒ throw new IllegalArgumentException("""Unknown value for field OrderProcessingState."type"""")
      }
    }
  }
}
