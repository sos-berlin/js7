package js7.data.orderwatch

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileWatchPatternDoesntMatchProblem
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
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
          expr("'/DIRECTORY'")))

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
          expr("'/DIRECTORY'")))

    "FileWatch complete" in:
      testJson[OrderWatch](
        FileWatch(
          OrderWatchPath("PATH"), WorkflowPath("WORKFLOW"), AgentPath("AGENT"),
          StringConstant("/DIRECTORY"),
          Some(SimplePattern("[a-z]+.csv")),
          Some(expr(
            """'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ "#F$js7EpochSecond-$orderWatchPath:$1"""")),
          Some(expr(s"""['DailyPlan', now(format='yyyy-MM-dd')]""")),
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

  private lazy val fileWatch = FileWatch(
    OrderWatchPath("FILE-WATCH"),
    WorkflowPath("WORKFLOW"),
    AgentPath("AGENT"),
    expr("'DIRECTORY'"),
    Some(SimplePattern("""file-(.+)\.csv""".r.pattern.pattern)),
    Some(expr(
      """"#" ++ now(format="yyyy-MM-dd", timezone="Pacific/Tahiti") ++ "#F-$orderWatchPath:$1"""")))

  private lazy val yyyymmdd = ZonedDateTime.now
    .withZoneSameInstant(ZoneId.of("Pacific/Tahiti"))
    .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))

  "relativePathToOrderId" in :
    assert(fileWatch.externalToOrderId(ExternalOrderName("X")) ==
      Left(FileWatchPatternDoesntMatchProblem(fileWatch.path / ExternalOrderName("X"))))

    // Test may fail at midnight, Tahiti time, due to date change
    assert(fileWatch.externalToOrderId(ExternalOrderName("file-ORDER.csv")) ==
      Right(OrderId(s"#$yyyymmdd#F-FILE-WATCH:ORDER")))
