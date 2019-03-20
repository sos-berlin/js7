package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.workflow.position.Position
import io.circe.generic.JsonCodec

@JsonCodec
final case class HistoricOutcome(position: Position, outcome: Outcome)
