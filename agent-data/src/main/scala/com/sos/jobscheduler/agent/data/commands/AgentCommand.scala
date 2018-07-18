package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.singletonCodec
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

  @JsonCodec
  final case class Batch(commands: Seq[AgentCommand])
  extends AgentCommand {
    type Response = Batch.Response

    override def toString = s"Batch(${commands.size} commands: ${commands take 3 map { _.getClass.getSimpleName } mkString ", "} ...)"
  }
  object Batch {
    sealed trait SingleResponse

    @JsonCodec
    final case class Succeeded(response: AgentCommand.Response)
    extends SingleResponse

    /** Failed commands let the web service succeed and are returns as Failed. */
    @JsonCodec
    final case class Failed(message: String) extends SingleResponse

    object SingleResponse {
      implicit val jsonFormat = TypedJsonCodec[SingleResponse](
        Subtype[Succeeded],
        Subtype[Failed])
    }

    @JsonCodec
    final case class Response(responses: Seq[SingleResponse])
    extends AgentCommand.Response {
      override def toString = {
        val succeeded = responses count { _.isInstanceOf[Succeeded] }
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"
      }
    }
  }

  case object Accepted extends AgentCommand.Response {
    implicit val jsonCodec: CirceCodec[Accepted.type] = singletonCodec(Accepted)
  }

  case object EmergencyStop extends TerminateOrAbort {
    /** The JVM is halted before responding. */
    type Response = Nothing
  }

  /** Some outer component has accepted the events until (including) the given `eventId`.
    * JobScheduler may delete these events to reduce the journal, keeping all events after `after`.
    */
  @JsonCodec
  final case class KeepEvents(after: EventId) extends OrderCommand {
    type Response = Accepted.type
  }

  case object NoOperation extends AgentCommand {
    type Response = Accepted.type
  }

  case object RegisterAsMaster extends AgentCommand {
    type Response = Accepted.type
  }

  sealed trait TerminateOrAbort extends AgentCommand

  @JsonCodec
  final case class Terminate(
    sigtermProcesses: Boolean = false,
    sigkillProcessesAfter: Option[FiniteDuration] = None)
  extends TerminateOrAbort {
    type Response = Accepted.type
  }

  object Terminate {
    val MaxDuration = 31 * 24.h
  }

  sealed trait OrderCommand extends AgentCommand

  sealed trait AttachOrDetachOrder extends OrderCommand

  @JsonCodec
  final case class AttachOrder(order: Order[Order.Idle], workflow: Workflow)
  extends AttachOrDetachOrder {
    order.workflowId.requireNonAnonymous()
    order.attachedToAgent.orThrow

    type Response = Accepted.type

    override def toShortString = s"AttachOrder($order)"
  }
  object AttachOrder {
    def apply(order: Order[Order.Idle], agentId: AgentId, workflow: Workflow) =
      new AttachOrder(
        order.copy(attachedTo = Some(Order.AttachedTo.Agent(agentId))),
        workflow)
  }

  @JsonCodec
  final case class DetachOrder(orderId: OrderId)
  extends AttachOrDetachOrder {
    type Response = Accepted.type
  }

  @JsonCodec
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
      Subtype[Batch],
      Subtype(EmergencyStop),
      Subtype[KeepEvents],
      Subtype(NoOperation),
      Subtype(RegisterAsMaster),
      Subtype[Terminate],
      Subtype[AttachOrder],
      Subtype[DetachOrder],
      Subtype[GetOrder],
      Subtype(GetOrderIds))

  implicit val ResponseJsonFormat: TypedJsonCodec[AgentCommand.Response] =
    TypedJsonCodec[AgentCommand.Response](
      Subtype.named[Batch.Response]("BatchResponse"),
      Subtype(Accepted))
}
