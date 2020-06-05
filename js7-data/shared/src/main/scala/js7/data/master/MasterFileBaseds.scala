package js7.data.master

import io.circe.Decoder
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils._
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.filebased.{FileBased, TypedPath}
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
object MasterFileBaseds
{
  val MasterTypedPathCompanions = Set[TypedPath.AnyCompanion](
    AgentRefPath,
    WorkflowPath)

  implicit val MasterTypedPathJsonCodec: CirceCodec[TypedPath] = TypedPath.jsonCodec(MasterTypedPathCompanions)

  implicit val jsonCodec = TypedJsonCodec[FileBased](
    Subtype[AgentRef],
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))

  private val typedPaths = AgentRefPath :: WorkflowPath :: Nil
  implicit val typedPathJsonDecoder: Decoder[TypedPath] = TypedPath.jsonDecoder(typedPaths.toKeyedMap(_.name).checked)
}
