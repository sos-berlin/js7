package js7.data.controller

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.circeutils.ScalaJsonCodecs.FiniteDurationJsonEncoder
import js7.base.time.{Timestamp, Timezone}
import js7.base.utils.IntelliJUtils.intelliJuseImport

final case class ControllerMetaState(
  controllerId: ControllerId,
  initiallyStartedAt: Timestamp,
  timezone: Timezone):

  def isDefined = this != ControllerMetaState.Undefined

object ControllerMetaState:
  val Undefined = ControllerMetaState(
    ControllerId("UNDEFINED-CONTROLLER-ID"),
    Timestamp.ofEpochMilli(0),
    Timezone("UTC"))

  implicit val jsonEncoder: Codec.AsObject[ControllerMetaState] =
    deriveRenamingCodec[ControllerMetaState](Map("startedAt" -> "initiallyStartedAt"))

  intelliJuseImport(FiniteDurationJsonEncoder)
