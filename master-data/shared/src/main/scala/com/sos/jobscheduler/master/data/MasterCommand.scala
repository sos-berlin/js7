package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.cluster.ClusterState.ClusterFailedOver
import com.sos.jobscheduler.data.command.{CancelMode, CommonCommand}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.crypt.SignedString
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.{TypedPath, VersionId}
import com.sos.jobscheduler.data.master.MasterFileBaseds.typedPathJsonDecoder
import com.sos.jobscheduler.data.order.OrderId
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
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
  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder, checkedJsonEncoder[Int], checkedJsonDecoder[Int], Problem, typedPathJsonDecoder))

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
    implicit val jsonEncoder: Encoder.AsObject[CancelOrder] = o =>
      JsonObject.fromIterable(
        ("orderId" -> o.orderId.asJson) ::
          (o.mode != CancelMode.Default).thenList("mode" -> o.mode.asJson))

    implicit val jsonDecoder: Decoder[CancelOrder] = c =>
      for {
        orderId <- c.get[OrderId]("orderId")
        mode <- c.get[Option[CancelMode]]("mode") map (_ getOrElse CancelMode.Default)
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
  final case class EmergencyStop(restart: Boolean = false) extends MasterCommand {
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

  /** Some outer component has accepted the events until (including) the given `eventId`.
    * JobScheduler may delete these events to reduce the journal, keeping all events after `after`.
    */
  final case class KeepEvents(after: EventId) extends MasterCommand {
    type Response = Response.Accepted
  }

  /** Shut down the Master properly. */
  final case class ShutDown(restart: Boolean = false) extends MasterCommand {
    type Response = Response.Accepted
  }
  object ShutDown {
    implicit val jsonEncoder: Encoder.AsObject[ShutDown] = o =>
      JsonObject.fromIterable(
        (o.restart).thenList("restart" -> Json.True))

    implicit val jsonDecoder: Decoder[ShutDown] = c =>
      for {
        restart <- c.get[Option[Boolean]]("restart") map (_ getOrElse false)
      } yield ShutDown(restart)
  }

  case object TakeSnapshot extends MasterCommand {
    type Response = Response.Accepted
  }

  final case class UpdateRepo(
    versionId: VersionId,
    change: Seq[SignedString] = Nil,
    delete: Seq[TypedPath] = Nil)
  extends MasterCommand {
    type Response = Response.Accepted

    def isEmpty = change.isEmpty && delete.isEmpty

    override def toString = s"UpdateRepo($versionId change=${change.size}× delete=${delete.size}×)"
  }

  final case class ReplaceRepo(versionId: VersionId, objects: Seq[SignedString])
  extends MasterCommand {
    type Response = Response.Accepted

    override def toString = s"ReplaceRepo($versionId, ${objects.size} objects)"
  }

  final case class ClusterAppointNodes(uris: Seq[Uri])
  extends MasterCommand {
    type Response = Response.Accepted
    assertThat(uris.size == 2)
  }

  // TODO Folgende Kommandos sind intern für die Knoten untereinander
  //  und sollen eine eigenes ClusterCommand bildern.
  final case class ClusterStartBackupNode(uris: Seq[Uri])
  extends MasterCommand {
    type Response = Response.Accepted
    assertThat(uris.size == 2)
  }

  final case class ClusterPrepareCoupling(activeUri: Uri, passiveUri: Uri)
  extends MasterCommand {
    type Response = Response.Accepted
    assertThat(activeUri != passiveUri)
    override def toString = s"ClusterPrepareCoupling($passiveUri follows $activeUri)"
  }

  final case class ClusterCouple(activeUri: Uri, passiveUri: Uri)
  extends MasterCommand {
    type Response = Response.Accepted
    assertThat(activeUri != passiveUri)
    override def toString = s"ClusterCouple($passiveUri couples with $activeUri)"
  }

  final case class ClusterRecouple(activeUri: Uri, passiveUri: Uri)
  extends MasterCommand {
    type Response = Response.Accepted
    assertThat(activeUri != passiveUri)
    override def toString = s"ClusterRecouple($passiveUri couples with $activeUri)"
  }

  case object ClusterSwitchOver
  extends MasterCommand {
    type Response = Response.Accepted
  }

  final case class ClusterInhibitActivation(duration: FiniteDuration)
  extends MasterCommand {
    type Response = ClusterInhibitActivation.Response
  }
  object ClusterInhibitActivation {
    final case class Response(failedOver: Option[ClusterFailedOver])
    extends MasterCommand.Response

    private implicit val x = deriveCodec[ClusterFailedOver]
    implicit val jsonCodec = deriveCodec[Response]
  }

  sealed trait Response

  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"),
      Subtype.named[ClusterInhibitActivation.Response]("ClusterInhibitActivationResponse"))
  }

  implicit val jsonCodec: TypedJsonCodec[MasterCommand] = TypedJsonCodec[MasterCommand](
    Subtype(deriveCodec[Batch]),
    Subtype[CancelOrder],
    Subtype(deriveCodec[ReplaceRepo]),
    Subtype(deriveCodec[UpdateRepo]),
    Subtype(NoOperation),
    Subtype(IssueTestEvent),
    Subtype[EmergencyStop],
    Subtype(deriveCodec[KeepEvents]),
    Subtype[ShutDown],
    Subtype(deriveCodec[ClusterAppointNodes]),
    Subtype(deriveCodec[ClusterStartBackupNode]),
    Subtype(deriveCodec[ClusterPrepareCoupling]),
    Subtype(deriveCodec[ClusterCouple]),
    Subtype(deriveCodec[ClusterRecouple]),
    Subtype(ClusterSwitchOver),
    Subtype(deriveCodec[ClusterInhibitActivation]),
    Subtype(TakeSnapshot))
}
