package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCodec, singletonCodec}
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.command.{CancelMode, CommonCommand}
import com.sos.jobscheduler.data.crypt.SignedString
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{Order, OrderId}
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

/**
 * @author Joacim Zschimmer
 */
sealed trait AgentCommand extends CommonCommand {
  type Response <: AgentCommand.Response
}

object AgentCommand extends CommonCommand.Companion
{
  intelliJuseImport(FiniteDurationJsonDecoder)

  protected type Command = AgentCommand

  trait Response
  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted {
      implicit val jsonCodec: CirceCodec[Accepted] = singletonCodec(Accepted)
    }
  }

  final case class Batch(commands: Seq[AgentCommand])
  extends AgentCommand with CommonBatch {
    type Response = Batch.Response
  }
  object Batch {
    final case class Response(responses: Seq[Checked[AgentCommand.Response]])
    extends AgentCommand.Response {
      override def toString = {
        val succeeded = responses count (_.isValid)
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"
      }
    }
  }

  final case class CancelOrder(orderId: OrderId, mode: CancelMode) extends OrderCommand {
    type Response = Response.Accepted
  }

  case object EmergencyStop extends TerminateOrAbort {
    /** The JVM is halted before responding. */
    type Response = Nothing
  }

  /** Some outer component has accepted the events until (including) the given `eventId`.
    * JobScheduler may delete these events to reduce the journal, keeping all events after `after`.
    */
  final case class KeepEvents(after: EventId) extends OrderCommand {
    type Response = Response.Accepted
  }

  case object NoOperation extends AgentCommand {
    type Response = Response.Accepted
  }

  /** Registers the Master identified by current User as a new Master and couples it.
    * The Agent Server starts a new Agent, dedicated to the Master.
    * Command may be given twice (in case of a sudden restart).
    */
  case object RegisterAsMaster extends AgentCommand {
    /**
      * @param agentRunId Use the value for `CoupleMaster`. */
    final case class Response(agentRunId: AgentRunId) extends AgentCommand.Response
  }

  /** Couples the registered Master identified by current User.
    * @param agentRunId Must be the value returned by `RegisterAsMaster`. */
  final case class CoupleMaster(agentRunId: AgentRunId, eventId: EventId) extends AgentCommand {
    type Response = Response.Accepted
  }

  case object TakeSnapshot extends AgentCommand {
    type Response = Response.Accepted
  }

  sealed trait TerminateOrAbort extends AgentCommand

  final case class Terminate(
    sigtermProcesses: Boolean = false,
    sigkillProcessesAfter: Option[FiniteDuration] = None)
  extends TerminateOrAbort {
    type Response = Response.Accepted
  }

  object Terminate {
    val MaxDuration = 31 * 24.h
  }

  sealed trait OrderCommand extends AgentCommand

  sealed trait AttachOrDetachOrder extends OrderCommand

  final case class AttachOrder(order: Order[Order.FreshOrReady], signedWorkflow: SignedString)
  extends AttachOrDetachOrder {
    order.workflowId.requireNonAnonymous()
    order.attached.orThrow

    type Response = Response.Accepted

    override def toShortString = s"AttachOrder($order)"
  }
  object AttachOrder {
    def apply(order: Order[Order.FreshOrReady], agentRefPath: AgentRefPath, signedWorkflow: SignedString) =
      new AttachOrder(
        order.copy(attachedState = Some(Order.Attached(agentRefPath))),
        signedWorkflow)
  }

  final case class DetachOrder(orderId: OrderId)
  extends AttachOrDetachOrder {
    type Response = Response.Accepted
  }

  final case class GetOrder(orderId: OrderId)
  extends OrderCommand {
    type Response = GetOrder.Response
  }
  object GetOrder {
    @JsonCodec
    final case class Response(order: Order[Order.State]) extends AgentCommand.Response
  }

  case object GetOrderIds extends OrderCommand {
    final case class Response(orderIds: Seq[OrderId]) extends AgentCommand.Response
  }

  case object GetOrders extends OrderCommand {
    final case class Response(orders: Seq[Order[Order.State]]) extends AgentCommand.Response
  }

  implicit val jsonCodec: TypedJsonCodec[AgentCommand] =
    TypedJsonCodec[AgentCommand](
      Subtype(deriveCodec[Batch]),
      Subtype(deriveCodec[CancelOrder]),
      Subtype(EmergencyStop),
      Subtype(deriveCodec[KeepEvents]),
      Subtype(NoOperation),
      Subtype(RegisterAsMaster),
      Subtype(deriveCodec[CoupleMaster]),
      Subtype(deriveCodec[Terminate]),
      Subtype(deriveCodec[AttachOrder]),
      Subtype(deriveCodec[DetachOrder]),
      Subtype(deriveCodec[GetOrder]),
      Subtype(TakeSnapshot),
      Subtype(GetOrderIds))

  implicit val responseJsonCodec: TypedJsonCodec[AgentCommand.Response] =
    TypedJsonCodec[AgentCommand.Response](
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"),
      Subtype(Response.Accepted),
      Subtype.named(deriveCodec[RegisterAsMaster.Response], "RegisterAsMasterResponse"))

  intelliJuseImport((checkedJsonEncoder[Int], checkedJsonDecoder[Int]))
}
