package js7.data_for_java.board

import java.time.Instant
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.JavaTimestamp.specific.*
import js7.data.board.{BoardPath, Notice, NoticeExpectation, NoticeId, NoticePlace}
import js7.data.order.OrderId
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class JNoticePlace(asScala: NoticePlace)
{
  @deprecated("Use noticeId", "2.4.0")
  @Deprecated
  @Nonnull
  def id: NoticeId =
    noticeId

  @Nonnull
  def noticeId: NoticeId =
    asScala.noticeId

  @Nonnull
  def notice: Optional[JNotice] =
    asScala.notice.map(JNotice(_)).toJava

  @Nonnull
  def expectation: Optional[JNoticeExpectation] =
    asScala.expectation.map(JNoticeExpectation(_)).toJava
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

final case class JNoticeExpectation(asScala: NoticeExpectation)
{
  def orderIds: java.util.Set[OrderId] =
    asScala.orderIds.asJava
}
