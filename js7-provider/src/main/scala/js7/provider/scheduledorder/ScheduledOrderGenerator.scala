package js7.provider.scheduledorder

import io.circe.Codec
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.item.{TrivialItemState, VersionedItem, VersionedItemId, VersionedItemPath}
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
extends VersionedItem, TrivialItemState[ScheduledOrderGenerator]:

  val companion: ScheduledOrderGenerator.type = ScheduledOrderGenerator

  def withId(id: VersionedItemId[ScheduledOrderGeneratorPath]): ScheduledOrderGenerator =
    reuseIfEqual(this, copy(id = id))


object ScheduledOrderGenerator
extends VersionedItem.Companion[ScheduledOrderGenerator],
  TrivialItemState.Companion[ScheduledOrderGenerator]:

  type Item = ScheduledOrderGenerator
  type Path = ScheduledOrderGeneratorPath

  val cls: Class[ScheduledOrderGenerator] = classOf[ScheduledOrderGenerator]
  val Path: VersionedItemPath.Companion[ScheduledOrderGeneratorPath] = ScheduledOrderGeneratorPath

  implicit def jsonCodec: Codec.AsObject[ScheduledOrderGenerator] = ???
