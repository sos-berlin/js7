package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPosition
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob

/**
  * @author Joacim Zschimmer
  */
final case class OrderStepEntry(
  orderId: OrderId,
  workflowPosition: WorkflowPosition,
  agentUri: String,
  jobName: Option[WorkflowJob.Name],
  startVariables: Map[String, String],
  startedAt: Timestamp,
  endedAt: Option[Timestamp] = None,
  returnCode: Option[ReturnCode] = None,
  endVariables: Option[Map[String, String]] = None,
  log: Option[String] = None)
