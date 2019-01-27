package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.implicits.checkedJsonCodec
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.command.{CancelMode, CommonCommand}
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.{SignedRepoObject, TypedPath, VersionId}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.data.MasterFileBaseds.typedPathJsonDecoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterCommand extends CommonCommand
{
  type Response <: MasterCommand.Response
}

object MasterCommand extends CommonCommand.Companion
{
  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder, checkedJsonCodec, Problem, typedPathJsonDecoder))

  protected type Command = MasterCommand

  final case class Batch(commands: Seq[MasterCommand])
  extends MasterCommand with CommonBatch {
    type Response = Batch.Response
  }
  object Batch {
    final case class Response(responses: Seq[Checked[MasterCommand.Response]])
    extends MasterCommand.Response with CommonBatch.Response {
      override def productPrefix = "BatchResponse"
    }
  }

  final case class CancelOrder(orderId: OrderId, mode: CancelMode) extends MasterCommand {
    type Response = Response.Accepted
  }
  object CancelOrder {
    implicit val jsonEncoder: ObjectEncoder[CancelOrder] = o ⇒
      JsonObject.fromIterable(
        ("orderId" → o.orderId.asJson) ::
          (o.mode != CancelMode.Default).thenList("mode" → o.mode.asJson))
    implicit val jsonDecoder: Decoder[CancelOrder] = c ⇒
      for {
        orderId ← c.get[OrderId]("orderId")
        mode ← c.get[Option[CancelMode]]("mode") map (_ getOrElse CancelMode.Default)
      } yield CancelOrder(orderId, mode)
  }

  sealed trait NoOperation extends MasterCommand
  case object NoOperation extends NoOperation {
    type Response = Response.Accepted
  }

  sealed trait IssueTestEvent extends MasterCommand
  /** For tests only. */
  case object IssueTestEvent extends IssueTestEvent {
    type Response = Response.Accepted
  }

  /** Master stops immediately with exit(). */
  case object EmergencyStop extends MasterCommand {
    type Response = Response.Accepted
  }

  /** Some outer component has accepted the events until (including) the given `eventId`.
    * JobScheduler may delete these events to reduce the journal, keeping all events after `after`.
    */
  final case class KeepEvents(after: EventId) extends MasterCommand {
    type Response = Response.Accepted
  }

  /** Test only. */
  final case class ScheduleOrdersEvery(every: FiniteDuration) extends MasterCommand

  /** Shut down the Master properly. */
  case object Terminate extends MasterCommand {
    type Response = Response.Accepted
  }

  case object TakeSnapshot extends MasterCommand {
    type Response = Response.Accepted
  }

  final case class UpdateRepo(
    change: Seq[SignedRepoObject] = Nil,
    delete: Seq[TypedPath] = Nil,
    versionId: Option[VersionId] = None)
  extends MasterCommand {
    type Response = Response.Accepted

    def isEmpty = versionId.isEmpty && change.isEmpty && delete.isEmpty

    override def toString = s"UpdateRepo(${versionId getOrElse ""}, change=${change.size}×, delete=${delete.size}×)"
  }

  /** Read the configured objects (workflows, agents) from the directory config/live. */
  @deprecated
  final case class ReadConfigurationDirectory(versionId: Option[VersionId]) extends MasterCommand {
    type Response = Response.Accepted
  }

  sealed trait Response

  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"))
  }

  implicit val jsonCodec: TypedJsonCodec[MasterCommand] = TypedJsonCodec[MasterCommand](
    Subtype(deriveCodec[Batch]),
    Subtype[CancelOrder],
    Subtype(deriveCodec[UpdateRepo]),
    Subtype(NoOperation),
    Subtype(IssueTestEvent),
    Subtype(EmergencyStop),
    Subtype(deriveCodec[KeepEvents]),
    Subtype(deriveCodec[ScheduleOrdersEvery]),
    Subtype(deriveCodec[ReadConfigurationDirectory]),
    Subtype(TakeSnapshot),
    Subtype(Terminate))
}
