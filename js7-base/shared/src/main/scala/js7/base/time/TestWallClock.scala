package js7.base.time

import scala.concurrent.duration.FiniteDuration

final class TestWallClock(start: Timestamp)
extends WallClock:

  private var _monotonicNanos = 0L
  private var _now = start

  def monotonicNanos() =
    _monotonicNanos

  def epochMilli() =
    _now.toEpochMilli

  def :=(ts: Timestamp): Unit =
    _monotonicNanos = (ts - start).toNanos
    _now = ts

  def +=(duration: FiniteDuration): Unit =
    _monotonicNanos += duration.toNanos
    _now += duration

  def -=(duration: FiniteDuration): Unit =
    _monotonicNanos -= duration.toNanos
    _now -= duration

  //override def toString =
  //  s"TestWallClock(${now()})"


object TestWallClock:
  def apply(start: Timestamp) =
    new TestWallClock(start)
