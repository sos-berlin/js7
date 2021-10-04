package js7.base.time

import scala.concurrent.duration.FiniteDuration

final class TestWallClock(startTimestamp: Timestamp)
extends WallClock
{
  private var _now = startTimestamp

  def epochMilli() =
    _now.toEpochMilli

  def :=(ts: Timestamp): Unit =
    _now = ts

  def +=(duration: FiniteDuration): Unit =
    _now += duration

  def -=(duration: FiniteDuration): Unit =
    _now -= duration

  override def toString =
    s"TestWallClock(${now()})"
}
