package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.session.SessionToken
import com.sos.jobscheduler.data.workflow.Workflow
import java.time.Duration
import scala.collection.immutable.Seq
import spray.json.DefaultJsonProtocol._

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
  trait Response

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
      implicit val jsonFormat = TypedJsonFormat[SingleResponse](name = "SingleResponse")(
        Subtype(jsonFormat1(Succeeded.apply), "Succeeded"),
        Subtype(jsonFormat1(Failed.apply), "Failed"))
    }

    final case class Response(responses: Seq[SingleResponse])
    extends AgentCommand.Response {
      override def toString = {
        val succeeded = responses count { _.isInstanceOf[Succeeded] }
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"
      }
    }

    object Response {
      implicit val jsonFormat = jsonFormat1(apply)
    }
  }

  case object Accepted extends AgentCommand.Response {
    implicit val jsonFormat = jsonFormat0(() ⇒ Accepted)
  }

  case object AbortImmediately extends TerminateOrAbort {
    /** The JVM is halted before responding. */
    type Response = Nothing
  }

  sealed trait SessionCommand extends AgentCommand

  case object Login extends SessionCommand {
    final case class Response(sessionToken: SessionToken) extends AgentCommand.Response
    object Response {
      implicit val jsonFormat = jsonFormat1(apply)
    }
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

  final case class Terminate(
    sigtermProcesses: Boolean = false,
    sigkillProcessesAfter: Option[Duration] = None)
  extends TerminateOrAbort {
    type Response = Accepted.type
  }

  object Terminate {
    val MaxDuration = 31 * 24.h
  }

  sealed trait OrderCommand extends AgentCommand

  sealed trait AttachOrDetachOrder extends OrderCommand

  final case class AttachOrder(order: Order[Order.Idle], workflow: Workflow)
  extends AttachOrDetachOrder {
    type Response = Accepted.type

    override def toShortString = s"AttachOrder($order,${workflow.path})"
  }

  final case class DetachOrder(orderId: OrderId)
  extends AttachOrDetachOrder {
    type Response = Accepted.type
  }

  final case class GetOrder(orderId: OrderId)
  extends OrderCommand {
    type Response = GetOrder.Response
  }
  object GetOrder {
    final case class Response(order: Order[Order.State]) extends AgentCommand.Response
  }

  case object GetOrderIds extends OrderCommand {
    final case class Response(orders: Seq[OrderId]) extends AgentCommand.Response
  }

  case object GetOrders extends OrderCommand {
    final case class Response(order: Seq[Order[Order.State]]) extends AgentCommand.Response
  }

  implicit val CommandJsonFormat: TypedJsonFormat[AgentCommand] =
    TypedJsonFormat.asLazy(
      TypedJsonFormat[AgentCommand](
        Subtype(jsonFormat1(Batch.apply)),
        Subtype(jsonFormat0(() ⇒ AbortImmediately)),
        Subtype(jsonFormat0(() ⇒ Login)),
        Subtype(jsonFormat0(() ⇒ Logout)),
        Subtype(jsonFormat0(() ⇒ NoOperation)),
        Subtype(jsonFormat0(() ⇒ RegisterAsMaster)),
        Subtype(jsonFormat2(Terminate.apply)),
        Subtype(jsonFormat2(AttachOrder.apply)),
        Subtype(jsonFormat1(DetachOrder.apply)),
        Subtype(jsonFormat1(GetOrder.apply)),
        Subtype(jsonFormat0(() ⇒ GetOrderIds))))

  implicit val ResponseJsonFormat: TypedJsonFormat[AgentCommand.Response] =
    TypedJsonFormat.asLazy(
      TypedJsonFormat[AgentCommand.Response]()(
        Subtype[Batch.Response]("BatchResponse"),
        Subtype[Accepted.type]("Accepted"),
        Subtype[Login.Response]("LoginResponse")))
}
