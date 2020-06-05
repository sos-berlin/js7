package js7.tests.history

import js7.base.time.Timestamp
import js7.base.web.Uri
import js7.data.job.ReturnCode
import js7.data.order.OrderId
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition

/**
  * @author Joacim Zschimmer
  */
final case class OrderStepEntry(
  orderId: OrderId,
  workflowPosition: WorkflowPosition,
  agentUri: Uri,
  jobName: Option[WorkflowJob.Name],
  startVariables: Map[String, String],
  startedAt: Timestamp,
  endedAt: Option[Timestamp] = None,
  returnCode: Option[ReturnCode] = None,
  endVariables: Option[Map[String, String]] = None,
  log: Option[String] = None)
