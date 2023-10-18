package js7.data.order

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.workflow.position.Position

final case class HistoricOutcome(position: Position, outcome: Outcome)


object HistoricOutcome:
  implicit val jsonCodec: Codec.AsObject[HistoricOutcome] = deriveCodec[HistoricOutcome]
