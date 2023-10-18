package js7.data_for_java.schedule

import java.time.{Duration, Instant}
import javax.annotation.Nonnull
import js7.base.time.JavaTimeConverters.{AsScalaDuration, AsScalaInstant}
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.TimeInterval
import js7.data_for_java.common.JavaWrapper
import scala.jdk.DurationConverters.ScalaDurationOps

final case class JTimeInterval(asScala: TimeInterval) extends JavaWrapper:
  type AsScala = TimeInterval

  def start: Instant =
    asScala.start.toInstant

  def duration: Duration =
    asScala.duration.toJava


object JTimeInterval:
  @Nonnull
  def of(
    @Nonnull start: Instant,
    @Nonnull duration: Duration)
  : JTimeInterval =
    JTimeInterval(
      TimeInterval(start.toTimestamp, duration.toFiniteDuration))
