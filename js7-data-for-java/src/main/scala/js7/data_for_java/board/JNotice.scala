package js7.data_for_java.board

import java.time.Instant
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.JavaTimestamp.specific.*
import js7.data.board.{BoardPath, Notice, NoticeId, NoticePlace}
import js7.data.order.OrderId
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class JNoticePlace(asScala: NoticePlace):

  @Nonnull
  def noticeId: NoticeId =
    asScala.noticeId

  @Nonnull
  def notice: Optional[JNotice] =
    asScala.notice.map(JNotice(_)).toJava

  def expectingOrderIds: java.util.Set[OrderId] =
    asScala.expectingOrderIds.asJava

final case class JNotice(asScala: Notice):
  @Nonnull
  def endOfLife: Optional[Instant] =
    asScala.endOfLife.map(_.toInstant).toJava


object JNotice:
  @Nonnull
  def of(
    @Nonnull id: NoticeId,
    @Nonnull boardPath: BoardPath,
    @Nonnull endOfLife: Optional[Instant])
  : JNotice =
    JNotice(Notice(id, boardPath, endOfLife.toScala.map(_.toTimestamp)))

@Deprecated
final case class JNoticeExpectation(orderIds: java.util.Set[OrderId])
