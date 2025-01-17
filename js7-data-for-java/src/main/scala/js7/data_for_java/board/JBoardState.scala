package js7.data_for_java.board

import java.util.Optional
import javax.annotation.Nonnull
import js7.data.board.{BoardPath, BoardState, GlobalBoard, PlannableBoard, PlannedNoticeKey}
import scala.jdk.OptionConverters.*

final case class JBoardState(asScala: BoardState):

  @Nonnull
  def path: BoardPath =
    asScala.path

  @Nonnull
  def board: JBoardItem =
    asScala.board match
      case o: GlobalBoard => JGlobalBoard(o)
      case o: PlannableBoard => JPlannableBoard(o)

  def idToNotice(noticeId: PlannedNoticeKey): Optional[JNoticePlace] =
    asScala.idToNotice
      .get(noticeId)
      .map(JNoticePlace(_))
      .toJava
