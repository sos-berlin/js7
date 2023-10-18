package js7.data_for_java.calendar

import io.vavr.control.Either as VEither
import java.time.Duration as JDuration
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.item.ItemRevision
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import scala.jdk.DurationConverters.ScalaDurationOps
import scala.jdk.OptionConverters.*

final case class JCalendar(asScala: Calendar)
extends JJsonable[JCalendar] with JUnsignedSimpleItem:

  type AsScala = Calendar
  protected def companion = JCalendar

  @Nonnull
  def path: CalendarPath =
    asScala.path

  @Nonnull
  def dateOffset: JDuration =
    asScala.dateOffset.toJava

  @Nonnull
  def orderIdToDatePattern: String =
    asScala.orderIdPattern

  @Nonnull
  def periodDatePattern: String =
    asScala.periodDatePattern

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]) =
    copy(asScala.withRevision(revision.toScala))


object JCalendar extends JJsonable.Companion[JCalendar]:
  type AsScala = Calendar

  @Nonnull
  def of(
    @Nonnull path: CalendarPath,
    @Nonnull dateOffset: JDuration,
    @Nonnull orderIdToDatePattern: String,
    @Nonnull periodDatePattern: String)
  : JCalendar =
    JCalendar(Calendar
      .checked(
        path,
        dateOffset = dateOffset.toFiniteDuration,
        orderIdToDatePattern = orderIdToDatePattern,
        periodDatePattern = periodDatePattern)
      .orThrow)

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JCalendar] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Calendar.jsonCodec
  protected def jsonDecoder = Calendar.jsonCodec
