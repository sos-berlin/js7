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
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.command.{CancelMode, CommonCommand}
import com.sos.jobscheduler.data.crypt.SignedString
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.{Order, OrderId}
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
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
        val succeeded = responses count (_.isRight)
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"
      }
    }
  }

  final case class CancelOrder(orderId: OrderId, mode: CancelMode) extends OrderCommand {
    type Response = Response.Accepted
  }

  final case class EmergencyStop(restart: Boolean = false) extends AgentCommand {
    /** The JVM is halted before responding. */
    type Response = Response.Accepted
  }
  object EmergencyStop {
    implicit val jsonEncoder: Encoder.AsObject[EmergencyStop] = o =>
      JsonObject.fromIterable(
        (o.restart).thenList("restart" -> Json.True))

    implicit val jsonDecoder: Decoder[EmergencyStop] = c =>
      for {
        restart <- c.get[Option[Boolean]]("restart") map (_ getOrElse false)
      } yield EmergencyStop(restart)
  }

  /** Some outer component no longer needs the events until (including) the given `untilEventId`.
    * JobScheduler may delete these events to reduce the journal,
    * keeping all events after `untilEventId`.
    */
  final case class ReleaseEvents(untilEventId: EventId) extends OrderCommand {
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

  sealed trait ShutdownOrAbort extends AgentCommand

  final case class ShutDown(
    sigtermProcesses: Boolean = false,
    sigkillProcessesAfter: Option[FiniteDuration] = None,
    restart: Boolean = false)
  extends ShutdownOrAbort {
    type Response = Response.Accepted
  }

  object ShutDown {
    val MaxDuration = 31 * 24.h

    implicit val jsonEncoder: Encoder.AsObject[ShutDown] = o =>
      JsonObject.fromIterable(
        o.sigtermProcesses.thenList("sigtermProcesses" -> Json.True) :::
        o.sigkillProcessesAfter.map(o => "sigkillProcessesAfter" -> o.asJson).toList :::
        o.restart.thenList("restart" -> Json.True))

    implicit val jsonDecoder: Decoder[ShutDown] = c =>
      for {
        sigtermProcesses <- c.get[Option[Boolean]]("sigtermProcesses") map (_ getOrElse false)
        sigkillProcessesAfter <- c.get[Option[FiniteDuration]]("sigkillProcessesAfter")
        restart <- c.get[Option[Boolean]]("restart") map (_ getOrElse false)
      } yield ShutDown(sigtermProcesses, sigkillProcessesAfter, restart)
  }

  sealed trait OrderCommand extends AgentCommand

  sealed trait AttachOrDetachOrder extends OrderCommand

  final case class AttachOrder(order: Order[Order.IsFreshOrReady], signedWorkflow: SignedString)
  extends AttachOrDetachOrder {
    order.workflowId.requireNonAnonymous()
    order.attached.orThrow

    type Response = Response.Accepted

    override def toShortString = s"AttachOrder($order)"
  }
  object AttachOrder {
    def apply(order: Order[Order.IsFreshOrReady], agentRefPath: AgentRefPath, signedWorkflow: SignedString) =
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
      Subtype[EmergencyStop],
      Subtype(deriveCodec[ReleaseEvents]),
      Subtype(NoOperation),
      Subtype(RegisterAsMaster),
      Subtype(deriveCodec[CoupleMaster]),
      Subtype[ShutDown],
      Subtype(deriveCodec[AttachOrder]),
      Subtype(deriveCodec[DetachOrder]),
      Subtype(deriveCodec[GetOrder]),
      Subtype(GetOrders),
      Subtype(TakeSnapshot),
      Subtype(GetOrderIds))

  implicit val responseJsonCodec: TypedJsonCodec[AgentCommand.Response] =
    TypedJsonCodec[AgentCommand.Response](
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"),
      Subtype(Response.Accepted),
      Subtype.named(deriveCodec[RegisterAsMaster.Response], "RegisterAsMasterResponse"))

  intelliJuseImport((checkedJsonEncoder[Int], checkedJsonDecoder[Int]))
}
