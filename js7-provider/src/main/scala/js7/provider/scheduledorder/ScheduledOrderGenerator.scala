package js7.provider.scheduledorder

import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.item.{TrivialItemState, VersionedItem, VersionedItemId}
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
with TrivialItemState[ScheduledOrderGenerator]:
  protected type Self = ScheduledOrderGenerator
  val item: ScheduledOrderGenerator = this

  val companion: ScheduledOrderGenerator.type = ScheduledOrderGenerator

  def withId(id: VersionedItemId[ScheduledOrderGeneratorPath]) =
    reuseIfEqual(this, copy(id = id))

object ScheduledOrderGenerator
extends VersionedItem.Companion[ScheduledOrderGenerator]
with TrivialItemState.Companion[ScheduledOrderGenerator]:
  type Item = ScheduledOrderGenerator
  type Path = ScheduledOrderGeneratorPath

  val cls = classOf[ScheduledOrderGenerator]
  val Path = ScheduledOrderGeneratorPath

  implicit def jsonCodec = ???
