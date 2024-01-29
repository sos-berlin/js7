package js7.base.time

import js7.base.utils.ScalaUtils.syntax.RichJavaClass

trait WallClock:

  def epochMilli(): Long

  final def now(): Timestamp =
    Timestamp.ofEpochMilli(epochMilli())

  protected def productPrefix: String =
    getClass.simpleScalaName

  override def toString = s"$productPrefix(${now()})"


object WallClock extends WallClock:

  def monotonicNanos() =
    System.nanoTime()

  override def epochMilli() =
    System.currentTimeMillis()

  def fixed(timestamp: Timestamp): WallClock =
    FixedWallclock(timestamp)

  private final case class FixedWallclock(fixed: Timestamp)
  extends WallClock:
    def epochMilli() = fixed.toEpochMilli
