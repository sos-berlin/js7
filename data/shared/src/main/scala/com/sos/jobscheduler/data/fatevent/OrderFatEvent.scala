package com.sos.jobscheduler.data.fatevent

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.order.{OrderId, Outcome}
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.data.workflow.instructions.Fork
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderFatEvent extends FatEvent {
  type Key = OrderId
}

object OrderFatEvent
{
  final case class OrderAddedFat(
    workflowPosition: WorkflowPosition,
    scheduledFor: Option[Timestamp],
    arguments: Map[String, String])
  extends OrderFatEvent

  final case class OrderForkedFat(workflowPosition: WorkflowPosition, children: Seq[OrderForkedFat.Child]) extends OrderFatEvent
  object OrderForkedFat {
    @JsonCodec
    final case class Child(branchId: Fork.Branch.Id, orderId: OrderId, arguments: Map[String, String])
  }

  final case class OrderJoinedFat(childOrderIds: Seq[OrderId], outcome: Outcome)
  extends OrderFatEvent

  final case class OrderFinishedFat(workflowPosition: WorkflowPosition) extends OrderFatEvent

  final case class OrderStoppedFat(workflowPosition: WorkflowPosition, outcome: Outcome.NotSucceeded) extends OrderFatEvent

  final case class OrderCanceledFat(workflowPosition: WorkflowPosition) extends OrderFatEvent

  final case class OrderProcessingStartedFat(
    workflowPosition: WorkflowPosition,
    agentRefPath: AgentRefPath,
    agentUri: Uri,
    jobName: Option[WorkflowJob.Name],
    keyValues: Map[String, String])
  extends OrderFatEvent

  final case class OrderProcessedFat(
    outcome: Outcome,
    keyValues: Map[String, String])
  extends OrderFatEvent

  sealed trait OrderStdWrittenFat extends OrderFatEvent {
    def stdoutOrStderr: StdoutOrStderr
    protected def chunk: String

    override def toString = s"${getClass.simpleScalaName}(${chunk.trim.truncateWithEllipsis(80, showLength = true)})"
  }
  object OrderStdWrittenFat {
    def apply(orderId: OrderId, t: StdoutOrStderr): String => OrderStdWrittenFat =
      t match {
        case Stdout => OrderStdoutWrittenFat
        case Stderr => OrderStderrWrittenFat
      }

    def unapply(o: OrderStdWrittenFat) = o match {
      case OrderStdoutWrittenFat(chunk) => Some((Stdout, chunk))
      case OrderStderrWrittenFat(chunk) => Some((Stderr, chunk))
    }
  }

  final case class OrderStdoutWrittenFat(chunk: String)
  extends OrderStdWrittenFat {
    def stdoutOrStderr = Stdout
  }

  final case class OrderStderrWrittenFat(chunk: String)
  extends OrderStdWrittenFat {
    def stdoutOrStderr = Stderr
  }

  implicit val jsonCodec = TypedJsonCodec[OrderFatEvent](
    Subtype(deriveCodec[OrderAddedFat]),
    Subtype(deriveCodec[OrderForkedFat]),
    Subtype(deriveCodec[OrderJoinedFat]),
    Subtype(deriveCodec[OrderFinishedFat]),
    Subtype(deriveCodec[OrderStoppedFat]),
    Subtype(deriveCodec[OrderCanceledFat]),
    Subtype(deriveCodec[OrderProcessingStartedFat]),
    Subtype(deriveCodec[OrderProcessedFat]),
    Subtype(deriveCodec[OrderStdoutWrittenFat]),
    Subtype(deriveCodec[OrderStderrWrittenFat]))
}
