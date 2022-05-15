package js7.data_for_java.board

import java.time.Instant
import javax.annotation.Nonnull
import js7.base.time.JavaTimeConverters.AsScalaInstant
import js7.base.time.JavaTimestamp.specific._
import js7.data.board.{BoardPath, Notice, NoticeExpectation, NoticeId, NoticePlace}
import js7.data.order.OrderId
import scala.jdk.CollectionConverters._

sealed trait JNoticePlace
{
  def asScala: NoticePlace

  @Nonnull
  final def id: NoticeId =
    asScala.id
}

object JNoticePlace
{
  def apply(noticeIdState: NoticePlace): JNoticePlace =
    noticeIdState match {
      case o: Notice => JNotice(o)
      case o: NoticeExpectation => JNoticeExpectation(o)
    }
}

final case class JNotice(asScala: Notice)
extends JNoticePlace
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
extends JNoticePlace
{
  def orderIds: java.util.Set[OrderId] =
    asScala.orderIds.asJava
}
