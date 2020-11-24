package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.workflow.position.Position

final case class HistoricOutcome(position: Position, outcome: Outcome)

object HistoricOutcome
{
  implicit val jsonCodec = deriveCodec[HistoricOutcome]
}
