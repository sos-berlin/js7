package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.oldruntime.OldSchedule

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  path: OrderGeneratorPath,
  workflowPath: WorkflowPath,
  variables: Map[String, String],
  schedule: OldSchedule)
