package js7.data_for_java.board

import io.vavr.control.{Either => VEither}
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.board.{Board, BoardPath}
import js7.data.item.ItemRevision
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.value.JExpression
import scala.jdk.OptionConverters._

final case class JBoard(asScala: Board)
extends JJsonable[JBoard] with JUnsignedSimpleItem
{
  protected type AsScala = Board
  protected def companion = JBoard

  @Nonnull
  def path: BoardPath =
    asScala.path

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]) =
    copy(asScala.withRevision(revision.toScala))
}

object JBoard extends JJsonable.Companion[JBoard]
{
  @Nonnull
  def of(
    @Nonnull lockPath: BoardPath,
    @Nonnull toNotice: JExpression,
    expectingOrderToNoticeId: JExpression,
    endOfLife: JExpression)
  : JBoard =
    JBoard(Board(lockPath, toNotice.asScala, expectingOrderToNoticeId.asScala, endOfLife.asScala))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JBoard] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Board.jsonCodec
  protected def jsonDecoder = Board.jsonCodec
}
