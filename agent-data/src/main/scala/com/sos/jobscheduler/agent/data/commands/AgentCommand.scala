package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.objectCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.session.SessionToken
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
    implicit val JsonCodec: CirceCodec[Accepted.type] = objectCodec(Accepted)
  }

  case object AbortImmediately extends TerminateOrAbort {
    /** The JVM is halted before responding. */
    type Response = Nothing
  }

  sealed trait SessionCommand extends AgentCommand

  case object Login extends SessionCommand {
    @JsonCodec
    final case class Response(sessionToken: SessionToken) extends AgentCommand.Response
  }

  case object Logout extends SessionCommand {
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
    type Response = Accepted.type

    override def toShortString = s"AttachOrder($order,${workflow.path})"
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
    final case class Response(orders: Seq[OrderId]) extends AgentCommand.Response
  }

  case object GetOrders extends OrderCommand {
    final case class Response(order: Seq[Order[Order.State]]) extends AgentCommand.Response
  }

  implicit val CommandJsonFormat: TypedJsonCodec[AgentCommand] =
    TypedJsonCodec[AgentCommand](
      Subtype[Batch],
      Subtype(AbortImmediately),
      Subtype(Login),
      Subtype(Logout),
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
      Subtype(Accepted),
      Subtype.named[Login.Response]("LoginResponse"))
}
