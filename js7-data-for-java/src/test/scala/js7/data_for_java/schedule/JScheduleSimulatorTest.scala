package js7.data_for_java.schedule

import io.circe.syntax.EncoderOps
import js7.base.test.OurTestSuite
import js7.data.execution.workflow.instructions.ScheduleTester

final class JScheduleSimulatorTest extends OurTestSuite
{
  "JScheduleSimulator" in {
    JScheduleSimulatorTester.testScheduleSimulator(
      ScheduleTester.schedule.asJson.toString)
  }
}
