package js7.journal.metering

import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.ScalaTime.*
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, NoKeyEvent}
import scala.concurrent.duration.FiniteDuration

sealed trait ResponsivenessEvent extends NoKeyEvent

object ResponsivenessEvent extends Event.KeyCompanion:
  type Key = NoKey
  def implicitSelf = this

  given TypedJsonCodec[ResponsivenessEvent] = TypedJsonCodec(
    Subtype(deriveCodecWithDefaults[InternalResponseTime]))


  final case class InternalResponseTime(delay: FiniteDuration, tooLong: Boolean = false)
  extends ResponsivenessEvent:
    override def toString = s"InternalResponseTime(${delay.pretty})"
