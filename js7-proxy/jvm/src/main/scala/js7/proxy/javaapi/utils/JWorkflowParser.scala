package js7.proxy.javaapi.utils

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.parser.WorkflowParser
import js7.proxy.javaapi.data.{JWorkflow, JWorkflowId}
import js7.proxy.javaapi.utils.VavrConverters._

@javaApi
object JWorkflowParser
{
  def parse(jWorkflowId: JWorkflowId, workflowNotation: String): VEither[Problem, JWorkflow] =
      WorkflowParser.parse(jWorkflowId.underlying, workflowNotation)
      .map(JWorkflow.apply)
      .toVavr
}
