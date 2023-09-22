package js7.data.order

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs.FiniteDurationJsonDecoder
import js7.base.time.{TimeInterval, Timestamp}
import js7.base.utils.IntelliJUtils.intelliJuseImport

/** State of an executing Cycle instruction.
 *
 * @param end End of cycling instruction, fixed
 * @param schemeIndex 0..(n-1)
 * @param periodIndex 0..(n-1)
 * @param index 1..n, or 0 after OrderCyclingPrepared or when irrelevant
 * @param next Scheduled OrderCycleStarted, or Timestamp.Epoch for immediate cycle start
 */
final case class CycleState(
  end: Timestamp,
  schemeIndex: Int = 0,
  periodIndex: Int = 0,
  index: Int,
  next: Timestamp)
{
  override def toString =
    s"CycleState(next=${next.pretty} index=$index schemeIndex=$schemeIndex period=$periodIndex end=$end)"
}

object CycleState
{
  def initial(timeInterval: TimeInterval): CycleState =
    CycleState(
      next = timeInterval.start,
      end = timeInterval.end,
      index = 0)

  implicit val jsonCodec: Codec.AsObject[CycleState] = deriveConfiguredCodec

  intelliJuseImport(FiniteDurationJsonDecoder)
}
