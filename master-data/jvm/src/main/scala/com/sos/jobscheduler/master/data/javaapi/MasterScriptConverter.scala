package js7.master.data.javaapi

import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.problem.JavaChecked
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPrinter}
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
