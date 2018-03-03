package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.filebased.FileBasedVersion
import com.sos.jobscheduler.data.order.Order
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

  final case class AddOrderIfNew(order: Order[Order.NotStarted]) extends MasterCommand {
    order.workflowPath.requireNonAnonymous()

    type MyResponse = Response.Accepted
  }

  final case class ScheduleOrdersEvery(every: FiniteDuration) extends MasterCommand

  case object Terminate extends MasterCommand {
    type MyResponse = Response.Accepted
  }

  final case class ReadConfigurationDirectory(version: FileBasedVersion) extends MasterCommand {
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
    Subtype(deriveCodec[AddOrderIfNew]),
    Subtype(deriveCodec[ScheduleOrdersEvery]),
    Subtype(deriveCodec[ReadConfigurationDirectory]),
    Subtype(Terminate))
}
