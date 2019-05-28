package com.sos.jobscheduler.master.data.events

import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.event.NoKeyEvent
import com.sos.jobscheduler.data.master.MasterId
import io.circe.generic.JsonCodec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait MasterEvent extends NoKeyEvent

object MasterEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  @JsonCodec
  final case class MasterStarted(masterId: MasterId, startedAt: Timestamp)
  extends MasterEvent

  @JsonCodec
  final case class MasterReady(timezone: String, totalRunningTime: FiniteDuration)
  extends MasterEvent

  case object MasterTestEvent extends MasterEvent

  implicit val jsonCodec: TypedJsonCodec[MasterEvent] = TypedJsonCodec(
    Subtype[MasterReady],
    Subtype(MasterTestEvent))
}
