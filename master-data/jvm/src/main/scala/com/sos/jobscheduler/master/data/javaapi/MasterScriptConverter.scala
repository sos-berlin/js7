package com.sos.jobscheduler.master.data.javaapi

import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import com.sos.jobscheduler.base.problem.JavaChecked
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPrinter}
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

  def workflowJsonToScript(jsonString: String): JavaChecked[String] =
    JavaChecked(
      jsonString.parseJsonChecked
        .flatMap(_.checkedAs[Workflow])
        .map(WorkflowPrinter.print))
}
