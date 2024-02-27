package js7.tests.controller.proxy.history

import java.time.Instant
import java.util.Optional
import js7.base.web.Uri
import js7.data.order.OrderId
import js7.data.value.Value
import js7.data_for_java.workflow.position.JWorkflowPosition

private[history] final case class OrderStepEntry(
  orderId: OrderId,
  workflowPosition: JWorkflowPosition,
  agentUri: Uri,
  jobName: Optional[String],
  startVariables: java.util.Map[String, Value],
  startedAt: Instant,
  endedAt: Optional[Instant] = Optional.empty,
  endVariables: Optional[java.util.Map[String, Value]] = Optional.empty,
  log: Optional[String] = Optional.empty)
