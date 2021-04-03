package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.SimplePattern
import js7.data.agent.AgentId
import js7.data.value.expression.ExpressionParser
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchTest extends AnyFreeSpec
{
  "JSON" - {
    "FileWatch minimum" in {
      val json = json"""{
        "TYPE": "FileWatch",
        "id": "ID",
        "workflowPath": "WORKFLOW",
        "agentId": "AGENT",
        "directory": "/DIRECTORY"
      }"""
      assert(json.as[OrderWatch].orThrow == FileWatch(OrderWatchId("ID"), WorkflowPath("WORKFLOW"), AgentId("AGENT"), "/DIRECTORY"))
    }

    "FileWatch complete" in {
      testJson[OrderWatch](
        FileWatch(
          OrderWatchId("ID"), WorkflowPath("WORKFLOW"), AgentId("AGENT"), "/DIRECTORY",
          Some(SimplePattern("[a-z]+.csv")),
          Some(ExpressionParser.parse(
            """'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ "#F$epochSecond-$orderWatchId:$1"""").orThrow),
          2.s),
        json"""{
          "TYPE": "FileWatch",
          "id": "ID",
          "workflowPath": "WORKFLOW",
          "agentId": "AGENT",
          "directory": "/DIRECTORY",
          "pattern": "[a-z]+.csv",
          "orderIdExpression": "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$$epochSecond-$$orderWatchId:$$1\"",
          "delay": 2
        }""")
    }
  }
}
