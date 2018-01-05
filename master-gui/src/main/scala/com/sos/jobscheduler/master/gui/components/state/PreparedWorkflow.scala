package com.sos.jobscheduler.master.gui.components.state

import com.sos.jobscheduler.data.workflow.WorkflowScript
import com.sos.jobscheduler.data.workflow.WorkflowScript.FlatStatement

/**
  * @author Joacim Zschimmer
  */
final case class PreparedWorkflow(script: WorkflowScript) {

  val nodeStatements = script.flatten.collect { case o: FlatStatement.Node ⇒ o }
}
