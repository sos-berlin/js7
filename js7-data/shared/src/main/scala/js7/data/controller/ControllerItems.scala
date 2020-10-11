package js7.data.controller

import io.circe.Decoder
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.{InventoryItem, ItemPath}
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
object ControllerItems
{
  val ControllerItemPathCompanions = Set[ItemPath.AnyCompanion](
    WorkflowPath)

  implicit val ControllerItemPathJsonCodec: CirceCodec[ItemPath] = ItemPath.jsonCodec(ControllerItemPathCompanions)

  implicit val jsonCodec = TypedJsonCodec[InventoryItem](
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))

  private val itemPaths = WorkflowPath :: Nil
  implicit val itemPathJsonDecoder: Decoder[ItemPath] = ItemPath.jsonDecoder(itemPaths.toKeyedMap(_.name).checked)
}
