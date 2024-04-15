package js7.data.calendar

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.{ItemRevision, UnsignedItemPath, UnsignedSimpleItem, UnsignedSimpleItemPath}
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

final case class Calendar(
  path: CalendarPath,
  dateOffset: FiniteDuration = Duration.Zero,
  orderIdPattern: String,
  periodDatePattern: String,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:

  protected type Self = Calendar

  val companion: Calendar.type = Calendar

  def checked: Checked[this.type] =
    ((!dateOffset.isNegative) !! Problem.pure("Invalid Calender arguments"))
      .rightAs(this)

  def rename(path: CalendarPath): Calendar =
    copy(path = path)

  def toInitialItemState: CalendarState =
    CalendarState(this)

  def withRevision(revision: Option[ItemRevision]): Calendar =
    copy(itemRevision = revision)


object Calendar extends UnsignedSimpleItem.Companion[Calendar]:
  val cls: Class[Calendar] = classOf[Calendar]

  @TestOnly
  private[calendar] val orderIdToDatePatternDefault = "#([^#]+)#.*"

  type Key = CalendarPath
  def Key: UnsignedSimpleItemPath.Companion[CalendarPath] = CalendarPath

  override type Path = CalendarPath
  override val Path: UnsignedItemPath.Companion[CalendarPath] = CalendarPath

  type ItemState = CalendarState

  @TestOnly
  def jocStandard(
    path: CalendarPath,
    dateOffset: FiniteDuration = Duration.Zero,
    itemRevision: Option[ItemRevision] = None)
  : Calendar =
    apply(path, dateOffset,
      orderIdToDatePattern = orderIdToDatePatternDefault,
      periodDatePattern = "yyyy-MM-dd",
      itemRevision = itemRevision)

  def apply(
    path: CalendarPath,
    dateOffset: FiniteDuration = Duration.Zero,
    orderIdToDatePattern: String,
    periodDatePattern: String,
    itemRevision: Option[ItemRevision] = None)
  : Calendar =
    checked(path, dateOffset, orderIdToDatePattern, periodDatePattern, itemRevision)
      .orThrow

  @TestOnly
  def daily(
    path: CalendarPath,
    dateOffset: FiniteDuration = Duration.Zero,
    itemRevision: Option[ItemRevision] = None)
  : Calendar =
    checked(
      path, dateOffset,
      orderIdToDatePattern = orderIdToDatePatternDefault,
      periodDatePattern = "yyyy-MM-dd",
      itemRevision
    ).orThrow

  def checked(
    path: CalendarPath,
    dateOffset: FiniteDuration,
    orderIdToDatePattern: String,
    periodDatePattern: String,
    itemRevision: Option[ItemRevision] = None)
  : Checked[Calendar] =
    new Calendar(path, dateOffset, orderIdToDatePattern, periodDatePattern, itemRevision)
      .checked

  implicit val jsonCodec: Codec.AsObject[Calendar] =
    deriveConfiguredCodec[Calendar].checked(_.checked)

  intelliJuseImport((FiniteDurationJsonEncoder))
