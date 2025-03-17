package js7.controller.command.executors

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.ChangePlanSchema
import js7.data.event.EventCalc
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.plan.PlanSchemaState

private[command] def changePlanSchemaExecutor = ToEventCalc[ChangePlanSchema]: cmd =>
  import cmd.planSchemaId
  EventCalc: coll =>
    for
      _ <- !planSchemaId.isGlobal !! Problem("The global PlanSchema cannot be changed")
      planSchema <- coll.aggregate.keyTo(PlanSchemaState).checked(planSchemaId)
      coll <- coll.add:
        planSchemaId <-: PlanSchemaChanged(
          namedValues =
            cmd.namedValues.flatMap(o => (o != planSchema.namedValues) ? o),
          finishedPlanRetentionPeriod =
            cmd.finishedPlanRetentionPeriod.flatMap:
              o => (o != planSchema.finishedPlanRetentionPeriod) ? o)
    yield
      coll