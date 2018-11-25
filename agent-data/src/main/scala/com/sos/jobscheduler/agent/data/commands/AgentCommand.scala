package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCodec, singletonCodec}
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentId
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.Workflow
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

/**
 * @author Joacim Zschimmer
 */
sealed trait AgentCommand {
  type Response <: AgentCommand.Response

  def toShortString = toString

  /**
   * true if toString returns a longer string than toShortString.
   */
  def toStringIsLonger = false
}

object AgentCommand {
  intelliJuseImport(FiniteDurationJsonDecoder)

  trait Response
  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted {
      implicit val jsonCodec: CirceCodec[Accepted] = singletonCodec(Accepted)
    }
  }

  final case class Batch(commands: Seq[AgentCommand])
  extends AgentCommand {
    type Response = Batch.Response

    override def toString = s"Batch(${commands.size} commands: ${commands take 3 map { _.getClass.getSimpleName } mkString ", "} ...)"
  }
  object Batch {
    sealed trait SingleResponse

    final case class Succeeded(response: AgentCommand.Response)
    extends SingleResponse

    /** Failed commands let the web service succeed and are returns as Failed. */
    final case class Failed(message: String) extends SingleResponse

    object SingleResponse {
      implicit val jsonFormat = TypedJsonCodec[SingleResponse](
        Subtype(deriveCodec[Succeeded]),
        Subtype(deriveCodec[Failed]))
    }

    final case class Response(responses: Seq[SingleResponse])
    extends AgentCommand.Response {
      override def toString = {
        val succeeded = responses count { _.isInstanceOf[Succeeded] }
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"
      }
    }
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

  case object RegisterAsMaster extends AgentCommand {
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

  final case class AttachOrder(order: Order[Order.FreshOrReady], workflow: Workflow)
  extends AttachOrDetachOrder {
    order.workflowId.requireNonAnonymous()
    order.attached.orThrow

    type Response = Response.Accepted

    override def toShortString = s"AttachOrder($order)"
  }
  object AttachOrder {
    def apply(order: Order[Order.FreshOrReady], agentId: AgentId, workflow: Workflow) =
      new AttachOrder(
        order.copy(attachedState = Some(Order.Attached(agentId))),
        workflow)
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

  implicit val CommandJsonFormat: TypedJsonCodec[AgentCommand] =
    TypedJsonCodec[AgentCommand](
      Subtype(deriveCodec[Batch]),
      Subtype(EmergencyStop),
      Subtype(deriveCodec[KeepEvents]),
      Subtype(NoOperation),
      Subtype(RegisterAsMaster),
      Subtype(deriveCodec[Terminate]),
      Subtype(deriveCodec[AttachOrder]),
      Subtype(deriveCodec[DetachOrder]),
      Subtype(deriveCodec[GetOrder]),
      Subtype(GetOrderIds))

  implicit val ResponseJsonFormat: TypedJsonCodec[AgentCommand.Response] =
    TypedJsonCodec[AgentCommand.Response](
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"),
      Subtype(Response.Accepted))
}
