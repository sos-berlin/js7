package js7.controller.data

import js7.base.circeutils.ScalaJsonCodecs.FiniteDurationJsonEncoder
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.controller.ControllerId

final case class ControllerMetaState(controllerId: ControllerId, startedAt: Timestamp, timezone: String)
{
  def isDefined = this != ControllerMetaState.Undefined
}

object ControllerMetaState
{
  val Undefined = ControllerMetaState(ControllerId("UNDEFINED-CONTROLLER-ID"), Timestamp.ofEpochMilli(0), timezone = "UTC")
  intelliJuseImport(FiniteDurationJsonEncoder)
}
