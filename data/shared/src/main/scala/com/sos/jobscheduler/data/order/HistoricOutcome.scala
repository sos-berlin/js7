package js7.data.order

import js7.data.workflow.position.Position
import io.circe.generic.JsonCodec

@JsonCodec
final case class HistoricOutcome(position: Position, outcome: Outcome)
