package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.{deriveCodecWithDefaults, toDecoderResult}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.time.Timestamp
import js7.data.event.{Event, KeyedEvent}

sealed trait NoticeEvent extends Event.IsKeyBase[NoticeEvent]:
  val keyCompanion: NoticeEvent.type = NoticeEvent


object NoticeEvent extends Event.CompanionForKey[BoardPath, NoticeEvent]:
  implicit def implicitSelf: NoticeEvent.type = this

  /** Notice posts via a PostNotice command (not the workflow instruction). */
  final case class NoticePosted(
    plannedNoticeKey: PlannedNoticeKey,
    endOfLife: Option[Timestamp] = None)
  extends NoticeEvent:
    def toNotice(boardPath: BoardPath): Notice =
      Notice(boardPath / plannedNoticeKey, endOfLife)

  object NoticePosted:
    def toKeyedEvent(notice: Notice): KeyedEvent[NoticePosted] =
      notice.boardPath <-: NoticePosted(notice.plannedNoticeKey, notice.endOfLife)

    private val jsonCodec: Codec.AsObject[NoticePosted] = deriveCodec[NoticePosted]

    given Encoder.AsObject[NoticePosted] = jsonCodec

    given Decoder[NoticePosted] = c =>
      val notice = c.downField("notice")
      if notice.succeeded then
        // COMPATIBLE with v2.7.3
        for
          noticeKey <- notice.get[NoticeKey]("id")
          noticeKey <- GlobalNoticeKey.checked(noticeKey).toDecoderResult(c.history)
          endOfLife <- notice.get[Option[Timestamp]]("endOfLife")
        yield
          NoticePosted(noticeKey, endOfLife)
      else
        jsonCodec(c)


  final case class NoticeDeleted(plannedNoticeKey: PlannedNoticeKey)
  extends NoticeEvent

  object NoticeDeleted:
    private val jsonCodec: Codec.AsObject[NoticeDeleted] = deriveCodec[NoticeDeleted]

    given Encoder.AsObject[NoticeDeleted] = jsonCodec

    given Decoder[NoticeDeleted] = c =>
      if c.get[Json]("noticeId").isRight then
        // COMPATIBLE with v2.7.3
        for
          noticeKey <- c.get[NoticeKey]("noticeId")
          globalNoticeKey <- GlobalNoticeKey.checked(noticeKey).toDecoderResult(c.history)
        yield
          NoticeDeleted(globalNoticeKey)
      else
        jsonCodec(c)


  final case class NoticeMoved(
    plannedNoticeKey: PlannedNoticeKey,
    newPlannedNoticeKey: PlannedNoticeKey,
    endOfLife: Option[Timestamp])
  extends NoticeEvent


  given TypedJsonCodec[NoticeEvent] = TypedJsonCodec(
    Subtype[NoticePosted],
    Subtype[NoticeDeleted],
    Subtype(deriveCodecWithDefaults[NoticeMoved]))
