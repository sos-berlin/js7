package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileWatchPatternDoesntMatchProblem
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.data.value.expression.Expression.{StringConstant, expr}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson

final class FileWatchTest extends OurTestSuite:

  "JSON" - {
    "FileWatch minimum" in:
      val json = json"""{
        "TYPE": "FileWatch",
        "path": "PATH",
        "workflowPath": "WORKFLOW",
        "agentPath": "AGENT",
        "directoryExpr": "'/DIRECTORY'"
      }"""
      assert(json.as[OrderWatch].orThrow ==
        FileWatch(OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          expr"'/DIRECTORY'"))

    "FileWatch, compatible with v2.0.1" in:
      val json = json"""{
        "TYPE": "FileWatch",
        "path": "PATH",
        "workflowPath": "WORKFLOW",
        "agentPath": "AGENT",
        "directoryExpr": "'/DIRECTORY'"
      }"""
      assert(json.as[OrderWatch].orThrow ==
        FileWatch(OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          expr"'/DIRECTORY'"))

    "FileWatch complete" in:
      testJson[OrderWatch](
        FileWatch(
          OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          StringConstant("/DIRECTORY"),
          Some(SimplePattern("[a-z]+.csv")),
          Some:
            expr"""'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ "#F$$js7EpochSecond-$$orderWatchPath:$$1" """,
          Some(expr"""['DailyPlan', now(format='yyyy-MM-dd')]"""),
          2.s),
        json"""{
          "TYPE": "FileWatch",
          "path": "PATH",
          "workflowPath": "WORKFLOW",
          "agentPath": "AGENT",
          "directoryExpr": "'/DIRECTORY'",
          "pattern": "[a-z]+.csv",
          "orderIdExpression": "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$$js7EpochSecond-$$orderWatchPath:$$1\"",
          "planIdExpr": "['DailyPlan', now(format='yyyy-MM-dd')]",
          "delay": 2
        }""")
  }

  "externalToOrderAndPlanId" in :
    val fileWatch = FileWatch(
      OrderWatchPath("FILE-WATCH"),
      WorkflowPath("WORKFLOW"),
      AgentPath("AGENT"),
      expr"'DIRECTORY'",
      Some(SimplePattern("""file-(.+)\.csv""".r.pattern.pattern)),
      orderIdExpression = Some:
        expr""" "#" ++ now(format="yyyy-MM-dd", timezone="Europe/Mariehamn") ++ "#F-$$orderWatchPath:$$1" """,
      planIdExpr = Some:
        expr""" [ 'DailyPlan', now(format="yyyy-MM-dd", timezone="Europe/Mariehamn") ]""")

    val timestamp = ts"2025-03-05T12:00:00Z"
    assert(fileWatch.externalToOrderAndPlanId(ExternalOrderName("X"), None, timestamp) ==
      Left(FileWatchPatternDoesntMatchProblem(fileWatch.path / ExternalOrderName("X"))))

    assert(fileWatch.externalToOrderAndPlanId(ExternalOrderName("file-ORDER.csv"), None, timestamp) ==
      Right(OrderId(s"#2025-03-05#F-FILE-WATCH:ORDER") -> PlanSchemaId("DailyPlan") / "2025-03-05"))
