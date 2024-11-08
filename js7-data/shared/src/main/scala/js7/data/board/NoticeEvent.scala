package js7.data.board

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.data.event.{Event, KeyedEvent}

sealed trait NoticeEvent extends Event.IsKeyBase[NoticeEvent]:
  val keyCompanion: NoticeEvent.type = NoticeEvent


object NoticeEvent extends Event.CompanionForKey[BoardPath, NoticeEvent]:
  implicit def implicitSelf: NoticeEvent.type = this

  /** Notice posts via a PostNotice command (not workflow instruction). */
  final case class NoticePosted(notice: NoticePosted.PostedNotice)
  extends NoticeEvent
  object NoticePosted:
    def toKeyedEvent(notice: Notice): KeyedEvent[NoticePosted] =
      notice.boardPath <-: NoticePosted(NoticePosted.PostedNotice(notice.id, notice.endOfLife))

    final case class PostedNotice(id: NoticeId, endOfLife: Option[Timestamp]):
      def toNotice(boardPath: BoardPath): Notice =
        Notice(id, boardPath, endOfLife)
    object PostedNotice:
      implicit val jsonCodec: Codec.AsObject[PostedNotice] = deriveCodec

  final case class NoticeDeleted(noticeId: NoticeId)
  extends NoticeEvent

  implicit val jsonCodec: TypedJsonCodec[NoticeEvent] = TypedJsonCodec(
    Subtype(deriveCodec[NoticePosted]),
    Subtype(deriveCodec[NoticeDeleted]))
