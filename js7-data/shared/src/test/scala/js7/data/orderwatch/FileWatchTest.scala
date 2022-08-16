package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.{expr, parseExpression}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson

final class FileWatchTest extends OurTestSuite
{
  "JSON" - {
    "FileWatch minimum" in {
      val json = json"""{
        "TYPE": "FileWatch",
        "path": "PATH",
        "workflowPath": "WORKFLOW",
        "agentPath": "AGENT",
        "directoryExpr": "'/DIRECTORY'"
      }"""
      assert(json.as[OrderWatch].orThrow ==
        FileWatch(OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          expr("'/DIRECTORY'")))
    }

    "FileWatch, compatible with v2.0.1" in {
      val json = json"""{
        "TYPE": "FileWatch",
        "path": "PATH",
        "workflowPath": "WORKFLOW",
        "agentPath": "AGENT",
        "directoryExpr": "'/DIRECTORY'"
      }"""
      assert(json.as[OrderWatch].orThrow ==
        FileWatch(OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          expr("'/DIRECTORY'")))
    }

    "FileWatch complete" in {
      testJson[OrderWatch](
        FileWatch(
          OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          StringConstant("/DIRECTORY"),
          Some(SimplePattern("[a-z]+.csv")),
          Some(parseExpression(
            """'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ "#F$js7EpochSecond-$orderWatchPath:$1"""").orThrow),
          2.s),
        json"""{
          "TYPE": "FileWatch",
          "path": "PATH",
          "workflowPath": "WORKFLOW",
          "agentPath": "AGENT",
          "directoryExpr": "'/DIRECTORY'",
          "pattern": "[a-z]+.csv",
          "orderIdExpression": "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$$js7EpochSecond-$$orderWatchPath:$$1\"",
          "delay": 2
        }""")
    }
  }
}
