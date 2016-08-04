package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat.Subtype
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
  def isInTask = false
  def isInProcess = false
}

object OrderProcessingState {

  case object NotPlanned
  extends OrderProcessingState

  final case class Planned(at: Instant)
  extends OrderProcessingState

  sealed trait Waiting
  extends OrderProcessingState {
    override def isWaiting = true
  }

  final case class Pending(at: Instant)
  extends Waiting

  sealed trait InTask
  extends OrderProcessingState {
    override def isInTask = true
    def taskId: TaskId
    def processClassPath: ProcessClassPath
    def agentUri: Option[AgentAddress]
  }

  final case class WaitingInTask(
    taskId: TaskId,
    processClassPath: ProcessClassPath,
    agentUri: Option[AgentAddress])
  extends InTask with Waiting

  final case class InTaskProcess(
    taskId: TaskId,
    processClassPath: ProcessClassPath,
    agentUri: Option[AgentAddress],
    since: Instant)
  extends InTask {
    override def isInProcess = true
  }

  final case class Setback(until: Instant)
  extends Waiting

  case object Blacklisted
  extends OrderProcessingState

  case object Suspended extends Waiting

  case object WaitingForOther extends Waiting

  implicit val OrderProcessingStateJsonFormat = TypedJsonFormat[OrderProcessingState](
    Subtype(jsonFormat0(() ⇒ NotPlanned)),
    Subtype(jsonFormat1(Planned)),
    Subtype(jsonFormat1(Pending)),
    Subtype(jsonFormat3(WaitingInTask)),
    Subtype(jsonFormat4(InTaskProcess)),
    Subtype(jsonFormat1(Setback)),
    Subtype(jsonFormat0(() ⇒ Blacklisted)),
    Subtype(jsonFormat0(() ⇒ Suspended)),
    Subtype(jsonFormat0(() ⇒ WaitingForOther)))
}
