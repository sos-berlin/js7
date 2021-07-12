package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event

sealed trait BoardEvent extends Event {
  type Key = BoardPath
}

object BoardEvent
{
  final case class NoticeDeleted(noticeId: NoticeId)
  extends BoardEvent

  implicit val jsonCodec = TypedJsonCodec[BoardEvent](
    Subtype(deriveCodec[NoticeDeleted]))
}
