package com.sos.jobscheduler.proxy.javaapi.utils

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.proxy.javaapi.data.{JWorkflow, JWorkflowId}
import com.sos.jobscheduler.proxy.javaapi.utils.VavrConversions._
import io.vavr.control.{Either => VEither}

@javaApi
object JWorkflowParser
{
  def parse(jWorkflowId: JWorkflowId, workflowNotation: String): VEither[Problem, JWorkflow] =
      WorkflowParser.parse(jWorkflowId.underlying, workflowNotation)
      .map(JWorkflow.apply)
      .asVavr
}
