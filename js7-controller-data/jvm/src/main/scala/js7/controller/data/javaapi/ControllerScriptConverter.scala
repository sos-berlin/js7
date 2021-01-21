package js7.controller.data.javaapi

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.problem.JavaChecked
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPrinter}

/**
  * Immutable.
  * @author Joacim Zschimmer
  */
final class ControllerScriptConverter
{
  def workflowScriptToJson(script: String): JavaChecked[String] =
    JavaChecked(
      WorkflowParser.parse(script).map(_.asJson.compactPrint))

  def workflowJsonToScript(jsonString: String): JavaChecked[String] =
    JavaChecked(
      jsonString.parseJson
        .flatMap(_.checkedAs[Workflow])
        .map(WorkflowPrinter.print))
}
