package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.jobscheduler.base.sprayjson.JavaTimeJsonFormats.implicits._
import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.jobnet.Jobnet
import com.sos.jobscheduler.data.order.{Order, OrderId}
import java.time.Duration
import scala.collection.immutable.Seq
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  object Response {
    /**
     * Serialization of all Response for a function AgentCommand => Response, which returns the unspecific type Response.
     */
    implicit object MyJsonFormat extends RootJsonWriter[Response] {
      def write(response: Response) = response match {
        case o: LoginResponse ⇒ o.toJson
        case EmptyResponse ⇒ EmptyResponse.toJson
        case o ⇒ throw new UnsupportedOperationException(s"Class ${o.getClass.getName} is not serializable as JSON")
      }
    }
  }

  case object AbortImmediately extends TerminateOrAbort {
    val SerialTypeName = "AbortImmediately"

    /** The JVM is halted before responding. */
    type Response = Nothing

    implicit val MyJsonFormat = jsonFormat0(() ⇒ AbortImmediately)
  }

  sealed trait SessionCommand extends AgentCommand

  case object Login extends SessionCommand {
    type Response = LoginResponse

    val SerialTypeName = "Login"
    implicit val jsonFormat = jsonFormat0(() ⇒ Login)
  }

  case object Logout extends SessionCommand {
    type Response = EmptyResponse.type

    val SerialTypeName = "Logout"
    implicit val jsonFormat = jsonFormat0(() ⇒ Logout)
  }

  case object NoOperation extends AgentCommand {
    type Response = EmptyResponse.type

    val SerialTypeName = "NoOperation"
    implicit val jsonFormat = jsonFormat0(() ⇒ NoOperation)
  }

  case object RegisterAsMaster extends AgentCommand {
    type Response = EmptyResponse.type

    val SerialTypeName = "RegisterAsMaster"
    implicit val jsonFormat = jsonFormat0(() ⇒ RegisterAsMaster)
  }

  sealed trait TerminateOrAbort extends AgentCommand

  final case class Terminate(
    sigtermProcesses: Boolean = false,
    sigkillProcessesAfter: Option[Duration] = None)
  extends TerminateOrAbort {
    type Response = EmptyResponse.type
  }

  object Terminate {
    val SerialTypeName = "Terminate"
    val MaxDuration = 31 * 24.h
    implicit val MyJsonFormat = jsonFormat2(apply)
  }



  sealed trait OrderCommand extends AgentCommand

  final case class AttachJobnet(jobnet: Jobnet) extends OrderCommand {
    type Response = EmptyResponse.type
  }

  object AttachJobnet {
    val SerialTypeName = "AttachJobnet"
    implicit val jsonFormat = jsonFormat1(apply)
  }

  final case class AttachOrder(order: Order[Order.Idle])
  extends OrderCommand {
    type Response = EmptyResponse.type
  }

  object AttachOrder {
    val SerialTypeName = "AttachOrder"
    implicit val jsonFormat = jsonFormat1(apply)
  }


  final case class DetachOrder(orderId: OrderId)
  extends OrderCommand {
    type Response = EmptyResponse.type
  }

  object DetachOrder {
    val SerialTypeName = "DetachOrder"
    implicit val jsonFormat = jsonFormat1(apply)
  }


  final case class GetOrder(orderId: OrderId) extends OrderCommand {
    type Response = GetOrder.Response
  }

  object GetOrder {
    val SerialTypeName = "GetOrder"
    implicit val jsonFormat = jsonFormat1(apply)
    final case class Response(order: Order[Order.State]) extends AgentCommand.Response
  }

  case object GetOrderIds extends OrderCommand {
    val SerialTypeName = "GetOrderIds"
    implicit val jsonFormat = jsonFormat0(() ⇒ GetOrderIds)
    final case class Response(orders: Seq[OrderId]) extends AgentCommand.Response
  }

  case object GetOrders extends OrderCommand {
    val SerialTypeName = "GetOrders"
    final case class Response(order: Seq[Order[Order.State]]) extends AgentCommand.Response
  }

  implicit val MyJsonFormat = TypedJsonFormat[AgentCommand](typeField = "$TYPE", shortenTypeOnlyValue = false)(
    Subtype[AbortImmediately.type](AbortImmediately.SerialTypeName),
    Subtype[Login.type](Login.SerialTypeName),
    Subtype[Logout.type](Logout.SerialTypeName),
    Subtype[NoOperation.type](NoOperation.SerialTypeName),
    Subtype[RegisterAsMaster.type](RegisterAsMaster.SerialTypeName),
    Subtype[Terminate](Terminate.SerialTypeName),
    Subtype[AttachJobnet](AttachJobnet.SerialTypeName),
    Subtype[AttachOrder](AttachOrder.SerialTypeName),
    Subtype[DetachOrder](DetachOrder.SerialTypeName),
    Subtype[GetOrder](GetOrder.SerialTypeName),
    Subtype[GetOrderIds.type](GetOrderIds.SerialTypeName))
}
