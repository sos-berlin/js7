package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.data.workflow.position.Position

final case class OrderStepHistory(steps: Seq[OrderStepHistory.StepOutcome])

object OrderStepHistory
{
  final case class StepOutcome(position: Position, outcome: Outcome)
}
