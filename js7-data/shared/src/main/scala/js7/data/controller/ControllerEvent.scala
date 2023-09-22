package js7.data.controller

import io.circe.derivation.ConfiguredCodec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.deriveRenamingCodec
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.{Timestamp, Timezone}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.event.NoKeyEvent
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerEvent extends NoKeyEvent

object ControllerEvent
{
  intelliJuseImport(FiniteDurationJsonEncoder)

  final case class ControllerInitialized(
    controllerId: ControllerId,
    initiallyStartedAt: Timestamp)
  extends ControllerEvent

  final case class ControllerReady(timezone: Timezone, totalRunningTime: FiniteDuration)
  extends ControllerEvent

  type ControllerShutDown = ControllerShutDown.type
  case object ControllerShutDown extends ControllerEvent

  case object ControllerTestEvent extends ControllerEvent

  implicit val jsonCodec: TypedJsonCodec[ControllerEvent] = TypedJsonCodec(
    Subtype(deriveRenamingCodec[ControllerInitialized](Map(
      "startedAt" -> "initiallyStartedAt"))),
    Subtype(deriveCodec[ControllerReady]),
    Subtype(ControllerShutDown),
    Subtype(ControllerTestEvent))
}
