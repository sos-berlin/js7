package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.FiniteDurationJsonDecoder
import js7.base.time.Timestamp
import js7.base.utils.IntelliJUtils.intelliJuseImport

/** State of an executing Cycle instruction.
 *
 * @param end End of Cycle instruction, does not change
 * @param schemeIndex 0..(n-1)
 * @param index 1..n, or 0 after OrderCyclingPrepared or when irrelevant
 * @param next Scheduled OrderCycleStarted, or Timestamp.Epoch for immediate cycle start
 */
final case class CycleState(
  end: Timestamp,
  schemeIndex: Int,
  index: Int,
  next: Timestamp)
{
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
  def initial(until: Timestamp, index: Int = 0) =
    CycleState(
      schemeIndex = 0,
      index = index,
      end = until,
      next = Timestamp.Epoch)

  implicit val jsonCodec = deriveCodec[CycleState]
  intelliJuseImport(FiniteDurationJsonDecoder)
}
