package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.data.event.{Event, KeyedEvent}

sealed trait BoardEvent extends Event {
  type Key = BoardPath
}

object BoardEvent
{
  /** Notice posts via a PostNotice command (not workflow instruction). */
  final case class NoticePosted(notice: NoticePosted.PostedNotice)
  extends BoardEvent
  object NoticePosted {
    def toKeyedEvent(notice: Notice): KeyedEvent[NoticePosted] =
      notice.boardPath <-: NoticePosted(NoticePosted.PostedNotice(notice.id, notice.endOfLife))

    final case class PostedNotice(id: NoticeId, endOfLife: Timestamp)
    {
      def toNotice(boardPath: BoardPath): Notice =
        Notice(id, boardPath, endOfLife)
    }
    object PostedNotice {
      implicit val jsonCodec = deriveCodec[PostedNotice]
    }
  }

  final case class NoticeDeleted(noticeId: NoticeId)
  extends BoardEvent

  implicit val jsonCodec = TypedJsonCodec[BoardEvent](
    Subtype(deriveCodec[NoticePosted]),
    Subtype(deriveCodec[NoticeDeleted]))
}
