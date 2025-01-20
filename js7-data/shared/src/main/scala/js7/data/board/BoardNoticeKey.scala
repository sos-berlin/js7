package js7.data.board

import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}

final case class BoardNoticeKey(boardPath: BoardPath, noticeKey: NoticeKey)

object BoardNoticeKey:

  given Encoder[BoardNoticeKey] = o =>
    Json.fromValues:
      Seq(o.boardPath.asJson) ++
        o.noticeKey.nonEmpty ? o.noticeKey.asJson

  given Decoder[BoardNoticeKey] = c =>
    c.as[Vector[String]].flatMap: vec =>
      toDecoderResult(c.history):
        for
          boardPath <- vec.checked(0).flatMap(BoardPath.checked)
          noticeKey <- vec.get(1).fold(Checked(NoticeKey.empty))(NoticeKey.checked)
        yield
          BoardNoticeKey(boardPath, noticeKey)
