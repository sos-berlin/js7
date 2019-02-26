package com.sos.jobscheduler.data.master

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import io.circe.Decoder

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
