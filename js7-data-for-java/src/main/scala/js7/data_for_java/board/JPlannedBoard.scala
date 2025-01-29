package js7.data_for_java.board

import js7.data.board.{NoticeKey, NoticePlace, PlannedBoardId}
import js7.data_for_java.common.MoreJavaConverters.asJava
import scala.jdk.CollectionConverters.*

final case class JPlannedBoard(
  id: PlannedBoardId,
  private val toNoticePlace_ : Map[NoticeKey, NoticePlace]):

  def toNoticePlace: java.util.Map[NoticeKey, JNoticePlace] =
    toNoticePlace_.view.mapValues(JNoticePlace(_)).asJava


object JPlannedBoard:

  def of(id: PlannedBoardId, toNoticePlace: java.util.Map[NoticeKey, JNoticePlace]): JPlannedBoard =
    JPlannedBoard(id, toNoticePlace.asScala.view.mapValues(_.asScala).toMap)
