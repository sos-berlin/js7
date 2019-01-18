package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import io.circe.Decoder

/**
  * @author Joacim Zschimmer
  */
object MasterFileBaseds
{
  implicit val jsonCodec = TypedJsonCodec[FileBased](
    Subtype[Agent],
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))

  private val typedPaths = AgentPath :: WorkflowPath :: Nil
  implicit val typedPathJsonDecoder: Decoder[TypedPath] = TypedPath.jsonDecoder(typedPaths.toKeyedMap(_.name).checked)
}
