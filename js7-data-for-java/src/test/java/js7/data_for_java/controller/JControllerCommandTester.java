package js7.data_for_java.controller;

import java.time.Duration;
import java.util.Optional;
import js7.data.controller.ControllerCommand.ChangePlan;
import js7.data.controller.ControllerCommand.ChangePlanSchema;
import js7.data.plan.PlanId;
import js7.data.plan.PlanKey;
import js7.data.plan.PlanSchemaId;
import js7.data.value.Value;
import js7.data_for_java.plan.JPlanStatus;

public class JControllerCommandTester {

    static void test() {
        ChangePlanSchema changePlanSchema = JControllerCommand.changePlanSchema(
            PlanSchemaId.of("DailyPlan"),
            Optional.of(
                java.util.Map.of("unknownPlansAreOpenFrom", Value.of("2025-02-26"))),
            Optional.of(
                Duration.ofDays(3)));

        ChangePlan changePlan = JControllerCommand.changePlan(
            new PlanId(PlanSchemaId.of("DailyPlan"), PlanKey.of("2025-02-26")),
            JPlanStatus.Closed());
    }
}
