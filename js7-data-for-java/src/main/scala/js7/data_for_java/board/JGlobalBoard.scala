package js7.data_for_java.board

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.board.{BoardPath, GlobalBoard}
import js7.data.item.ItemRevision
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.value.JExpression
import scala.jdk.OptionConverters.*

final case class JGlobalBoard(asScala: GlobalBoard)
extends JJsonable[JGlobalBoard], JUnsignedSimpleItem:

  type AsScala = GlobalBoard
  protected def companion = JGlobalBoard

  @Nonnull
  def path: BoardPath =
    asScala.path

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JGlobalBoard =
    copy(asScala.withRevision(revision.toScala))


object JGlobalBoard extends JJsonable.Companion[JGlobalBoard]:
  type AsScala = GlobalBoard

  @Nonnull
  def of(
    @Nonnull path: BoardPath,
    @Nonnull toNotice: JExpression,
    @Nonnull expectingOrderToNoticeId: JExpression,
    @Nonnull endOfLife: JExpression)
  : JGlobalBoard =
    JGlobalBoard(GlobalBoard(path, toNotice.asScala, expectingOrderToNoticeId.asScala, endOfLife.asScala))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JGlobalBoard] =
    super.fromJson(jsonString)

  protected def jsonEncoder = GlobalBoard.jsonCodec
  protected def jsonDecoder = GlobalBoard.jsonCodec
