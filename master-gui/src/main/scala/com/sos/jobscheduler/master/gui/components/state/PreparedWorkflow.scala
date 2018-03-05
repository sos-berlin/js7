package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
final case class PreparedWorkflow(id: WorkflowId, workflow: Workflow)
