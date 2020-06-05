package js7.provider.scheduledorder

import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.filebased.{FileBased, FileBasedId}
import js7.data.workflow.WorkflowPath
import js7.provider.scheduledorder.oldruntime.OldSchedule

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
