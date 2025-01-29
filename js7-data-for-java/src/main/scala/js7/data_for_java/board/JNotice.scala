package js7.data_for_java.board

import java.time.Instant
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.JavaTimestamp.specific.*
import js7.data.board.{BoardPath, Notice, NoticeId, NoticeKey, NoticePlace, PlannedNoticeKey}
import js7.data.order.OrderId
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class JNoticePlace(asScala: NoticePlace):

  @Nonnull
  def notice: Optional[JNotice] =
    asScala.notice.map(JNotice(_)).toJava

  def expectingOrderIds: java.util.Set[OrderId] =
    asScala.expectingOrderIds.asJava

  def isAnnounced: Boolean =
    asScala.isAnnounced


object JNoticePlace:

  def of(
    notice: Optional[Notice],
    expectingOrderIds: java.util.Set[OrderId],
    isAnnounced: Boolean,
    isInConsumption: Boolean,
    consumptionCount: Int)
  : JNoticePlace =
    JNoticePlace:
      NoticePlace(
        notice.toScala,
        expectingOrderIds.asScala.toSet,
        isAnnounced,
        isInConsumption,
        consumptionCount)


final case class JNotice(asScala: Notice):

  @Nonnull
  def noticeKey: NoticeKey =
    asScala.noticeKey

  @Nonnull
  def endOfLife: Optional[Instant] =
    asScala.endOfLife.map(_.toInstant).toJava


object JNotice:

  @Nonnull
  def of(
    @Nonnull id: PlannedNoticeKey,
    @Nonnull boardPath: BoardPath,
    @Nonnull endOfLife: Optional[Instant])
  : JNotice =
    JNotice(Notice(
      boardPath / id,
      endOfLife.toScala.map(_.toTimestamp)))

  @Nonnull
  def of(
    @Nonnull id: NoticeId,
    @Nonnull endOfLife: Optional[Instant])
  : JNotice =
    JNotice(Notice(
      id.boardPath / id.plannedNoticeKey,
      endOfLife.toScala.map(_.toTimestamp)))


@Deprecated
final case class JNoticeExpectation(orderIds: java.util.Set[OrderId])
