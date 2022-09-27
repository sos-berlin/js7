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

final case class JNoticePlace(asScala: NoticePlace)
{
  @Nonnull
  def noticeId: NoticeId =
    asScala.noticeId

  @Nonnull
  def notice: Optional[JNotice] =
    asScala.notice.map(JNotice(_)).toJava

  @Deprecated
  @deprecated("Use expectingOrderIds", "v2.5")
  @Nonnull
  def expectation: Optional[JNoticeExpectation] =
    if (asScala.expectingOrderIds.isEmpty)
      Optional.empty
    else
      Optional.of(JNoticeExpectation(asScala.expectingOrderIds.asJava))

  def expectingOrderIds: java.util.Set[OrderId] =
    asScala.expectingOrderIds.asJava
}

final case class JNotice(asScala: Notice)
{
  @Nonnull
  def endOfLife: Instant =
    asScala.endOfLife.toInstant
}

object JNotice
{
  @Nonnull
  def of(
    @Nonnull id: NoticeId,
    @Nonnull boardPath: BoardPath,
    @Nonnull endOfLife: Instant)
  : JNotice =
    JNotice(Notice(id, boardPath, endOfLife.toTimestamp))
}

@Deprecated
final case class JNoticeExpectation(orderIds: java.util.Set[OrderId])
