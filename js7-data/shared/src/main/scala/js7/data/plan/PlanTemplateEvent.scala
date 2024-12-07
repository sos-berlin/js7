package js7.data.plan

import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
import js7.data.value.NamedValues

sealed trait PlanTemplateEvent extends Event:
  type KeyBase = PlanTemplateEvent

  val keyCompanion: PlanTemplateEvent.type = PlanTemplateEvent


object PlanTemplateEvent extends Event.KeyCompanion[PlanTemplateEvent]:
  type Key = PlanTemplateId

  given implicitSelf: PlanTemplateEvent.type = this

  final case class PlanTemplateChanged(namedValues: NamedValues)
  extends PlanTemplateEvent

  given TypedJsonCodec[PlanTemplateEvent] = TypedJsonCodec(
    Subtype(deriveCodecWithDefaults[PlanTemplateChanged]))
