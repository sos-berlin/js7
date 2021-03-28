package js7.provider.scheduledorder

import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.item.{VersionedItem, VersionedItemId}
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import js7.provider.scheduledorder.oldruntime.OldSchedule

// FOR DEVELOPMENT ONLY !!!
/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  id: VersionedItemId[ScheduledOrderGeneratorPath],
  workflowPath: WorkflowPath,
  arguments: NamedValues,
  schedule: OldSchedule)
extends VersionedItem
{
  type Self = ScheduledOrderGenerator

  val companion = ScheduledOrderGenerator

  def withId(id: VersionedItemId[ScheduledOrderGeneratorPath]) =
    reuseIfEqual(this, copy(id = id))
}

object ScheduledOrderGenerator extends VersionedItem.Companion[ScheduledOrderGenerator] {
  type Item = ScheduledOrderGenerator
  type Path = ScheduledOrderGeneratorPath

  val cls = classOf[ScheduledOrderGenerator]
  val itemPathCompanion = ScheduledOrderGeneratorPath

  implicit def jsonCodec = ???
}
