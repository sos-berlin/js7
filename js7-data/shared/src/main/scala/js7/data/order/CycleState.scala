package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.FiniteDurationJsonDecoder
import js7.base.time.{TimeInterval, Timestamp}
import js7.base.utils.IntelliJUtils.intelliJuseImport

/** State of an executing Cycle instruction.
 *
 * @param end End of cycling instruction, fixed
 * @param schemeIndex 0..(n-1)
 * @param index 1..n, or 0 after OrderCyclingPrepared or when irrelevant
 * @param next Scheduled OrderCycleStarted, or Timestamp.Epoch for immediate cycle start
 */
final case class CycleState(
  //TODO insert start time, only in case the clock is set before start:  start: Timestamp,
  // next must not be before start
  end: Timestamp,
  schemeIndex: Int,
  index: Int,
  next: Timestamp)
{
  //def timeInterval = TimeInterval(start, end - start)

  def reduceNext(now: Timestamp) =
    if (next <= now)
      copy(next = Timestamp.Epoch)
    else
      this

  override def toString =
    s"CycleState(next=${next.pretty} index=$index schemeIndex=$schemeIndex end=$end)"
}

object CycleState
{
  def initial(timeInterval: TimeInterval) =
    CycleState(
      //start = timeInterval.start,
      next = timeInterval.start,
      end = timeInterval.end,
      schemeIndex = 0,
      index = 0)

  implicit val jsonCodec = deriveCodec[CycleState]
  intelliJuseImport(FiniteDurationJsonDecoder)
}
