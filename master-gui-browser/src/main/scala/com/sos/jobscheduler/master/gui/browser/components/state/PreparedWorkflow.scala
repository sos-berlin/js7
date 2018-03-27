package com.sos.jobscheduler.master.gui.browser.components.state

import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId}

/**
  * @author Joacim Zschimmer
  */
final case class PreparedWorkflow(id: WorkflowId, workflow: Workflow)
