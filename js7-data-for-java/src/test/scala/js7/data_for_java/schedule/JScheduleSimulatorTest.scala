package js7.data_for_java.schedule

import io.circe.syntax.EncoderOps
import js7.data.workflow.instructions.ScheduleTest
import org.scalatest.freespec.AnyFreeSpec

final class JScheduleSimulatorTest extends AnyFreeSpec
{
  "JScheduleSimulator" in {
    JScheduleSimulatorTester.testScheduleSimulator(
      ScheduleTest.exampleSchedule.asJson.toString)
  }
}
