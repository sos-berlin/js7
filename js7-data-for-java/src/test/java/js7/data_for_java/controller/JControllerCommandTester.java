package js7.data_for_java.controller;

import js7.data.controller.ControllerCommand.ChangePlan;
import js7.data.controller.ControllerCommand.ChangePlanSchema;
import js7.data.plan.Plan;
import js7.data.plan.PlanId;
import js7.data.plan.PlanKey;
import js7.data.plan.PlanSchemaId;
import js7.data.value.Value;

public class JControllerCommandTester {

    static void test() {
        ChangePlanSchema changePlanSchema = JControllerCommand.changePlanSchema(
            PlanSchemaId.of("DailyPlan"),
            java.util.Map.of("openingDay", Value.of("2025-02-26")));

        ChangePlan changePlan = JControllerCommand.changePlan(
            new PlanId(PlanSchemaId.of("DailyPlan"), PlanKey.of("2025-02-26")),
            Plan.Status$.Closed);
    }
}
