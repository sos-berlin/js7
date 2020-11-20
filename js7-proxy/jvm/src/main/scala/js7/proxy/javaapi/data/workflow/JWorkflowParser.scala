package js7.proxy.javaapi.data.workflow

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.WorkflowParser
import js7.proxy.javaapi.data.common.VavrConverters._

@javaApi
object JWorkflowParser
{
  def parse(jWorkflowId: JWorkflowId, workflowNotation: String): VEither[Problem, JWorkflow] =
      WorkflowParser.parse(jWorkflowId.asScala, workflowNotation)
      .map(JWorkflow.apply)
      .toVavr
}
