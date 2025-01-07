package js7.data.plan

import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
import js7.data.value.NamedValues

sealed trait PlanSchemaEvent extends Event:
  type KeyBase = PlanSchemaEvent

  val keyCompanion: PlanSchemaEvent.type = PlanSchemaEvent


object PlanSchemaEvent extends Event.KeyCompanion[PlanSchemaEvent]:
  type Key = PlanSchemaId

  given implicitSelf: PlanSchemaEvent.type = this

  final case class PlanSchemaChanged(namedValues: NamedValues)
  extends PlanSchemaEvent

  given TypedJsonCodec[PlanSchemaEvent] = TypedJsonCodec(
    Subtype(deriveCodecWithDefaults[PlanSchemaChanged]))
