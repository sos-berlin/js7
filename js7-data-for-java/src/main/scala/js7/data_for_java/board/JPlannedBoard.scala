package js7.data_for_java.board

import js7.base.utils.MoreJavaConverters.asJava
import js7.data.board.{NoticeKey, PlannedBoard, PlannedBoardId}
import js7.data_for_java.common.JavaWrapper
import scala.jdk.CollectionConverters.*

final case class JPlannedBoard(asScala: PlannedBoard) extends JavaWrapper:

  type AsScala = PlannedBoard

  def id: PlannedBoardId =
    asScala.id

  def toNoticePlace: java.util.Map[NoticeKey, JNoticePlace] =
    asScala.toNoticePlace.view.mapValues(JNoticePlace(_)).asJava


object JPlannedBoard:

  def of(id: PlannedBoardId, toNoticePlace: java.util.Map[NoticeKey, JNoticePlace]): JPlannedBoard =
    JPlannedBoard:
      PlannedBoard(id, toNoticePlace.asScala.view.mapValues(_.asScala).toMap)
