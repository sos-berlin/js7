package js7.base.time

trait WallClock
{
  def epochMilli(): Long

  final def now(): Timestamp =
    Timestamp.ofEpochMilli(epochMilli())

  override def toString =
    s"WallClock(${now()})"
}

object WallClock extends WallClock
{
  override def epochMilli() =
    System.currentTimeMillis()

  def fixed(timestamp: Timestamp): WallClock =
    Fixed(timestamp)

  private final case class Fixed(now_ : Timestamp)
  extends WallClock
  {
    def epochMilli() = now_.toEpochMilli
  }
}
