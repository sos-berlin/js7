package js7.controller.command.executors

import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.CommandEventConverter
import js7.data.controller.ControllerCommand.ChangePlan
import js7.data.event.EventCalc
import js7.data.plan.PlanSchemaState

private[command] def changePlanExecutor: CommandEventConverter[ChangePlan] =
  CommandEventConverter.checked[ChangePlan]: (cmd, controllerState) =>
    for
      planSchemaState <- controllerState.keyTo(PlanSchemaState).checked(cmd.planId.planSchemaId)
      plan <- planSchemaState.plan(cmd.planId.planKey)
      coll <- plan.changePlanStatusEvents(
        cmd.status,
        EventCalc.now(),
        planSchemaState.finishedPlanRetentionPeriod)
    yield
      coll
