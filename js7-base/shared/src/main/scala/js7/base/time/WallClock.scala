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
}
