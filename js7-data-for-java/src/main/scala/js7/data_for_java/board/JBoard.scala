package js7.data_for_java.board

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.board.{Board, BoardPath}
import js7.data.item.ItemRevision
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.value.JExpression
import scala.jdk.OptionConverters.*

final case class JBoard(asScala: Board)
extends JJsonable[JBoard] with JUnsignedSimpleItem
{
  type AsScala = Board
  protected def companion = JBoard

  @Nonnull
  def path: BoardPath =
    asScala.path

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JBoard =
    copy(asScala.withRevision(revision.toScala))
}

object JBoard extends JJsonable.Companion[JBoard]
{
  @Nonnull
  def of(
    @Nonnull path: BoardPath,
    @Nonnull toNotice: JExpression,
    @Nonnull expectingOrderToNoticeId: JExpression,
    @Nonnull endOfLife: JExpression)
  : JBoard =
    JBoard(Board(path, toNotice.asScala, expectingOrderToNoticeId.asScala, endOfLife.asScala))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JBoard] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Board.jsonCodec
  protected def jsonDecoder = Board.jsonCodec
}
