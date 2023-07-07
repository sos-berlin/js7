package js7.base.time

import scala.concurrent.duration.FiniteDuration

final class TestWallClock(start: Timestamp)
extends WallClock
{
  private var _now = start

  def epochMilli() =
    _now.toEpochMilli

  def :=(ts: Timestamp): Unit =
    _now = ts

  def +=(duration: FiniteDuration): Unit =
    _now += duration

  def -=(duration: FiniteDuration): Unit =
    _now -= duration

  //override def toString =
  //  s"TestWallClock(${now()})"
}

object TestWallClock
{
  def apply(start: Timestamp) =
    new TestWallClock(start)
}
