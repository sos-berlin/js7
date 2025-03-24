package js7.data.plan

import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
import js7.data.value.NamedValues
import scala.concurrent.duration.FiniteDuration

sealed trait PlanSchemaEvent extends Event.IsKeyBase[PlanSchemaEvent]:

  val keyCompanion: PlanSchemaEvent.type = PlanSchemaEvent


object PlanSchemaEvent extends Event.CompanionForKey[PlanSchemaId, PlanSchemaEvent]:

  given implicitSelf: PlanSchemaEvent.type = this

  final case class PlanSchemaChanged(
    finishedPlanRetentionPeriod: Option[FiniteDuration] = None,
    namedValues: Option[NamedValues] = None)
  extends PlanSchemaEvent

  given TypedJsonCodec[PlanSchemaEvent] = TypedJsonCodec(
    Subtype(deriveCodecWithDefaults[PlanSchemaChanged]))
