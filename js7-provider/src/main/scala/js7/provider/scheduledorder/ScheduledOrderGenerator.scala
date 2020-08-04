package js7.provider.scheduledorder

import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.item.{InventoryItem, ItemId}
import js7.data.workflow.WorkflowPath
import js7.provider.scheduledorder.oldruntime.OldSchedule

// FOR DEVELOPMENT ONLY !!!
/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGenerator(
  id: ItemId[ScheduledOrderGeneratorPath],
  workflowPath: WorkflowPath,
  arguments: Map[String, String],
  schedule: OldSchedule)
extends InventoryItem
{
  type Self = ScheduledOrderGenerator

  val companion = ScheduledOrderGenerator

  def withId(id: ItemId[ScheduledOrderGeneratorPath]) = reuseIfEqual(this, copy(id = id))
}

object ScheduledOrderGenerator extends InventoryItem.Companion[ScheduledOrderGenerator] {
  type ThisItem = ScheduledOrderGenerator
  type Path = ScheduledOrderGeneratorPath

  def typedPathCompanion = ScheduledOrderGeneratorPath
}
