package js7.tests.jobs

import cats.syntax.traverse.*
import js7.base.time.ScalaTime.{DurationRichInt, RichFiniteDuration}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.order.Outcome
import js7.data.value.expression.Expression.NumericConstant
import js7.data.workflow.instructions.Execute
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.launcher.internal.InternalJob.JobContext
import scala.concurrent.duration.FiniteDuration

final class SleepJob(jobContext: JobContext) extends InternalJob
{
  import jobContext.clock

  def toOrderProcess(step: Step) =
    OrderProcess(
      step.arguments
        .get("sleep")
        .fold_(Right(0.s), _.asDuration)
        .traverse(duration =>
          clock.sleep(duration).when(duration.isPositive)
            .as(Outcome.succeeded))
        .map(Outcome.Completed.fromChecked))
}

object SleepJob extends InternalJob.Companion[SleepJob]
{
  def sleep(agentPath: AgentPath, duration: FiniteDuration): Execute =
    execute(
      agentPath,
      arguments = Map(
        "sleep" -> NumericConstant(duration.toBigDecimalSeconds)))
}
