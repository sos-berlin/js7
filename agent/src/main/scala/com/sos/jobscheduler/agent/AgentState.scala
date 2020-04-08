package com.sos.jobscheduler.agent

import com.sos.jobscheduler.data.event.{EventId, JournalState}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
final case class AgentState(
  eventId: EventId,
  journalState: JournalState,
  idToOrder: Map[OrderId, Order[Order.State]],
  idToWorkflow: Map[WorkflowId, Workflow])
