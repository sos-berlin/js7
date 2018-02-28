package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.data.filebased.FileBased
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.oldruntime.OldSchedule

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  path: ScheduledOrderGeneratorPath,
  workflowPath: WorkflowPath,
  variables: Map[String, String],
  schedule: OldSchedule)
extends FileBased
{
  type Self = ScheduledOrderGenerator

  def companion = ScheduledOrderGenerator
}

object ScheduledOrderGenerator extends FileBased.Companion[ScheduledOrderGenerator] {
  type ThisFileBased = ScheduledOrderGenerator
  type ThisTypedPath = ScheduledOrderGeneratorPath

  def typedPathCompanion = ScheduledOrderGeneratorPath
}
