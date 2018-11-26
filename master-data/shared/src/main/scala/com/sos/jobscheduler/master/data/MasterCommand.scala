package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.data.MasterCommand._
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterCommand {
  type MyResponse <: Response
}

object MasterCommand {
  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))

  final case class CancelOrder(orderId: OrderId) extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  /** Master stops immediately with exit(). */
  case object EmergencyStop extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  /** Some outer component has accepted the events until (including) the given `eventId`.
    * JobScheduler may delete these events to reduce the journal, keeping all events after `after`.
    */
  final case class KeepEvents(after: EventId) extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  /** Test only. */
  final case class ScheduleOrdersEvery(every: FiniteDuration) extends MasterCommand

  /** Shut down the Master properly. */
  case object Terminate extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  /** Read the configured objects (workflows, agents) from the directory config/live. */
  final case class ReadConfigurationDirectory(versionId: Option[VersionId]) extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  sealed trait Response

  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted

    implicit val ResponseJsonCodec = TypedJsonCodec[Response](
      Subtype(Accepted))
  }

  implicit val jsonCodec = TypedJsonCodec[MasterCommand](
    Subtype(deriveCodec[CancelOrder]),
    Subtype(EmergencyStop),
    Subtype(deriveCodec[KeepEvents]),
    Subtype(deriveCodec[ScheduleOrdersEvery]),
    Subtype(deriveCodec[ReadConfigurationDirectory]),
    Subtype(Terminate))
}
