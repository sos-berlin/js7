package js7.data.controller

import io.circe.Decoder
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.item.{InventoryItem, TypedPath}
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
object ControllerItems
{
  val ControllerTypedPathCompanions = Set[TypedPath.AnyCompanion](
    AgentRefPath,
    WorkflowPath)

  implicit val ControllerTypedPathJsonCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(ControllerTypedPathCompanions)

  implicit val jsonCodec = TypedJsonCodec[InventoryItem](
    Subtype[AgentRef],
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))

  private val typedPaths = AgentRefPath :: WorkflowPath :: Nil
  implicit val typedPathJsonDecoder: Decoder[TypedPath] = TypedPath.jsonDecoder(typedPaths.toKeyedMap(_.name).checked)
}
