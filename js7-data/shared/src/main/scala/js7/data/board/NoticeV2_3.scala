package js7.data.board

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp

// COMPATIBLE with v2.3
final case class NoticeV2_3(id: NoticeId, endOfLife: Timestamp)
{
  def toNotice(boardPath: BoardPath): Notice =
    Notice(id, boardPath, endOfLife)
}

object NoticeV2_3
{
  implicit val jsonCodec: Codec.AsObject[NoticeV2_3] = deriveCodec[NoticeV2_3]
}
