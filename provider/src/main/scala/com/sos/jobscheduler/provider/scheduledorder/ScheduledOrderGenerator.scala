package com.sos.jobscheduler.provider.scheduledorder

import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.provider.scheduledorder.oldruntime.OldSchedule

// FOR DEVELOPMENT ONLY !!!
/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  id: FileBasedId[ScheduledOrderGeneratorPath],
  workflowPath: WorkflowPath,
  arguments: Map[String, String],
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
