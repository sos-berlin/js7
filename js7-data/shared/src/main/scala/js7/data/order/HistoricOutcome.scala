package js7.data.order

import io.circe.generic.JsonCodec
import js7.data.workflow.position.Position

@JsonCodec
final case class HistoricOutcome(position: Position, outcome: Outcome)
