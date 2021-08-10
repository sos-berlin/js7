package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event

sealed trait BoardEvent extends Event {
  type Key = BoardPath
}

object BoardEvent
{
  /** Notice posts via a PostNotice command (not workflow instruction). */
  final case class NoticePosted(notice: Notice)
  extends BoardEvent

  final case class NoticeDeleted(noticeId: NoticeId)
  extends BoardEvent

  implicit val jsonCodec = TypedJsonCodec[BoardEvent](
    Subtype(deriveCodec[NoticePosted]),
    Subtype(deriveCodec[NoticeDeleted]))
}
