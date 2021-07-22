package js7.base.time

trait WallClock
{
  def epochMilli(): Long

  final def now(): Timestamp =
    Timestamp.ofEpochMilli(epochMilli())
}

object WallClock extends WallClock
{
  override def epochMilli() =
    System.currentTimeMillis()

  def fixed(timestamp: Timestamp): WallClock =
    new Fixed(timestamp.toEpochMilli)

  private final case class Fixed(fixed: Long) extends WallClock
  {
    def epochMilli() = fixed
  }
}
