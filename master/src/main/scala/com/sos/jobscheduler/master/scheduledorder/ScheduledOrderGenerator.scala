package com.sos.jobscheduler.master.scheduledorder

import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.oldruntime.OldSchedule

// FOR DEVELOPMENT ONLY !!!
/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  id: FileBasedId[ScheduledOrderGeneratorPath],
  workflowPath: WorkflowPath,
  variables: Map[String, String],
  schedule: OldSchedule)
extends FileBased
{
  type Self = ScheduledOrderGenerator

  val companion = ScheduledOrderGenerator

  def withId(id: FileBasedId[ScheduledOrderGeneratorPath]) = reuseIfEqual(this, copy(id = id))
}

object ScheduledOrderGenerator extends FileBased.Companion[ScheduledOrderGenerator] {
  type ThisFileBased = ScheduledOrderGenerator
  type Path = ScheduledOrderGeneratorPath

  def typedPathCompanion = ScheduledOrderGeneratorPath
}
