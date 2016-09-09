package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.agent.AgentAddress
import com.sos.scheduler.engine.data.job.TaskId
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import java.time.Instant
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderProcessingState {
  def isWaiting = false
  def isInProcess = false
}

object OrderProcessingState {

  //private val Companions = List(NotPlanned, Planned, Pending, WaitingInTask, InTaskProcess, Setback, WaitingForOther, Blacklisted)

  sealed trait Companion {
    def name = getClass.getSimpleName stripSuffix "$"
  }

  // NotPlanned

  case object NotPlanned
  extends OrderProcessingState with Companion

  final case class Planned(at: Instant)
  extends OrderProcessingState

  // Planned

  case object Planned extends Companion

  // Waiting

  sealed trait Waiting
  extends OrderProcessingState {
    override def isWaiting = true
  }

  // Pending

  final case class Pending(at: Instant)
  extends Waiting

  case object Pending extends Companion

  // InTask

  sealed trait InTask
  extends OrderProcessingState {
    def taskId: TaskId
    def processClassPath: ProcessClassPath
  }

  case object InTask extends Companion

  // WaintingInTask

  final case class WaitingInTask(
    taskId: TaskId,
    processClassPath: ProcessClassPath)
  extends InTask with Waiting

  case object WaitingInTask extends Companion

  // InTaskProcess

  final case class InTaskProcess(
    taskId: TaskId,
    processClassPath: ProcessClassPath,
    agentUri: Option[AgentAddress],
    since: Instant)
  extends InTask {
    override def isInProcess = true
  }

  case object InTaskProcess extends Companion

  // Setback

  final case class Setback(until: Instant)
  extends Waiting

  case object Setback extends Companion

  // WaitingForOther

  case object WaitingForOther extends Waiting with Companion

  // Blacklisted

  case object Blacklisted
  extends OrderProcessingState with Companion

  //

  implicit val typedJsonFormat = TypedJsonFormat[OrderProcessingState](
    Subtype(jsonFormat0(() ⇒ NotPlanned)),
    Subtype(jsonFormat1(Planned.apply)),
    Subtype(jsonFormat1(Pending.apply)),
    Subtype(jsonFormat2(WaitingInTask.apply)),
    Subtype(jsonFormat4(InTaskProcess.apply)),
    Subtype(jsonFormat1(Setback.apply)),
    Subtype(jsonFormat0(() ⇒ WaitingForOther)),
    Subtype(jsonFormat0(() ⇒ Blacklisted)))
}
