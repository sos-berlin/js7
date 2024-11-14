package js7.data_for_java.board

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.board.{BoardPath, PlannableBoard}
import js7.data.item.ItemRevision
import js7.data_for_java.common.JJsonable
import scala.jdk.OptionConverters.*

final case class JPlannableBoard(asScala: PlannableBoard)
extends JJsonable[JPlannableBoard], JBoardItem:

  type AsScala = PlannableBoard
  protected def companion = JPlannableBoard

  @Nonnull
  def path: BoardPath =
    asScala.path

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JPlannableBoard =
    copy(asScala.withRevision(revision.toScala))


object JPlannableBoard extends JJsonable.Companion[JPlannableBoard]:
  type AsScala = PlannableBoard

  @Nonnull
  def of(@Nonnull path: BoardPath)
  : JPlannableBoard =
    JPlannableBoard(PlannableBoard(path))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JPlannableBoard] =
    super.fromJson(jsonString)

  protected def jsonEncoder = PlannableBoard.jsonCodec
  protected def jsonDecoder = PlannableBoard.jsonCodec
