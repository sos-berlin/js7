package js7.data.event

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait SnapshotMeta

object SnapshotMeta
{
  final case class SnapshotEventId(eventId: EventId)
  extends SnapshotMeta

  implicit val jsonCodec = TypedJsonCodec[SnapshotMeta](
    Subtype(deriveCodec[SnapshotEventId]))
}
