package com.sos.jobscheduler.master.data.javaapi

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.JavaChecked
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPrinter}
import io.circe.parser.{parse ⇒ parseJson}
import io.circe.syntax.EncoderOps

/**
  * Immutable.
  * @author Joacim Zschimmer
  */
final class MasterScriptConverter
{
  def workflowScriptToJson(script: String): JavaChecked[String] =
    JavaChecked(
      WorkflowParser.parse(script).map(_.asJson.compactPrint))

  def workflowJsonToScript(json: String): JavaChecked[String] =
    JavaChecked(
      parseJson(json).toChecked
        .flatMap(_.as[Workflow].toChecked)
        .map(WorkflowPrinter.print))
}
