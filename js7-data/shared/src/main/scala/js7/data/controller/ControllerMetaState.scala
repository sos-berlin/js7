package js7.data.controller

import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.circeutils.ScalaJsonCodecs.FiniteDurationJsonEncoder
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport

final case class ControllerMetaState(
  controllerId: ControllerId,
  initiallyStartedAt: Timestamp,
  timezone: String)
{
  def isDefined = this != ControllerMetaState.Undefined
}

object ControllerMetaState
{
  val Undefined = ControllerMetaState(
    ControllerId("UNDEFINED-CONTROLLER-ID"),
    Timestamp.ofEpochMilli(0),
    "UTC")

  implicit val jsonEncoder = deriveRenamingCodec[ControllerMetaState](Map(
    "startedAt" -> "initiallyStartedAt"))

  intelliJuseImport(FiniteDurationJsonEncoder)
}
