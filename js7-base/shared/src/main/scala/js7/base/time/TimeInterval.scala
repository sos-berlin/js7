package js7.base.time

import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import scala.concurrent.duration._

final case class TimeInterval(start: Timestamp, duration: FiniteDuration)
{
  assertThat(!duration.isNegative)

  def end = start + duration

  def contains(timestamp: Timestamp): Boolean =
    start <= timestamp && !endsBefore(timestamp)

  def endsBefore(timestamp: Timestamp) =
    end <= timestamp

  override def toString = s"TimeInterval($start, ${duration.pretty})"
}

object TimeInterval
{
  val never = TimeInterval(Timestamp.ofEpochMilli(Long.MinValue), 0.s)
  val alwaysSinceEpoch = TimeInterval(Timestamp.Epoch, FiniteDuration.MaxValue)
}
