package js7.data.order

import js7.data.workflow.position.Position

final case class OrderStepHistory(steps: Seq[OrderStepHistory.StepOutcome])

object OrderStepHistory
{
  final case class StepOutcome(position: Position, outcome: Outcome)
}
