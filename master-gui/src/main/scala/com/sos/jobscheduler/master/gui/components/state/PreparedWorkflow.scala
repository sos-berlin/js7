package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
final case class PreparedWorkflow(path: WorkflowPath, workflow: Workflow)
