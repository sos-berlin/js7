package js7.controller.command.executors

import js7.base.utils.ScalaUtils.syntax.*
import js7.controller.command.ControllerCommandToEventCalc.ToEventCalc
import js7.data.controller.ControllerCommand.ChangePlan
import js7.data.event.EventCalc
import js7.data.plan.PlanSchemaState

private[command] def changePlanExecutor = ToEventCalc[ChangePlan]: cmd =>
  EventCalc: coll =>
    for
      planSchemaState <- coll.aggregate.keyTo(PlanSchemaState).checked(cmd.planId.planSchemaId)
      plan <- planSchemaState.plan(cmd.planId.planKey)
      coll <- coll.addChecked:
        plan.changePlanStatusEvents(
          cmd.status,
          coll.context.now,
          planSchemaState.finishedPlanRetentionPeriod)
    yield
      coll
