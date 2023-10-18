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
  override def epochMilli() =
    System.currentTimeMillis()

  def fixed(timestamp: Timestamp): WallClock =
    Fixed(timestamp)

  private final case class Fixed(now_ : Timestamp)
  extends WallClock:
    def epochMilli() = now_.toEpochMilli
