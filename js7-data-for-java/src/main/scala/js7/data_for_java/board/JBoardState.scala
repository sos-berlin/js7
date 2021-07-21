package js7.data_for_java.board

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.board.{BoardPath, BoardState, NoticeId}
import scala.jdk.OptionConverters._

final case class JBoardState(asScala: BoardState)
{
  @Nonnull
  def path: BoardPath =
    asScala.path

  @Nonnull
  def board: JBoard =
    JBoard(asScala.board)

  def idToNotice(noticeId: NoticeId): Optional[JNoticePlace] =
    asScala.idToNotice
      .get(noticeId)
      .map(JNoticePlace(_))
      .toJava
}
