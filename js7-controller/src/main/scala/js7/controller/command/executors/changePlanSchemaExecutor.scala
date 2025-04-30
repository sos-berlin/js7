package js7.controller.command.executors

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.ChangePlanSchema
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.plan.PlanSchemaState

private[command] def changePlanSchemaExecutor: CommandEventConverter[ChangePlanSchema] =
  CommandEventConverter.checked[ChangePlanSchema]: (cmd, controllerState) =>
    import cmd.planSchemaId
    for
      _ <- !planSchemaId.isGlobal !! Problem("The global PlanSchema cannot be changed")
      planSchema <- controllerState.keyTo(PlanSchemaState).checked(planSchemaId)
    yield
      Some:
        planSchemaId <-: PlanSchemaChanged(
          namedValues =
            cmd.namedValues.flatMap(o => (o != planSchema.namedValues) ? o),
          finishedPlanRetentionPeriod =
            cmd.finishedPlanRetentionPeriod.flatMap:
              o => (o != planSchema.finishedPlanRetentionPeriod) ? o)
