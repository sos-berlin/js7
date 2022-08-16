package js7.agent.scheduler.order

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import js7.agent.scheduler.order.FileWatchManager.relativePathToOrderId
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.order.OrderId
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.ExpressionParser.{expr, parseExpression}
import js7.data.workflow.WorkflowPath

final class FileWatchManagerTest extends OurTestSuite
{
  private lazy val fileWatch = FileWatch(
    OrderWatchPath("FILE-WATCH"),
    WorkflowPath("WORKFLOW"),
    AgentPath("AGENT"),
    expr("'DIRECTORY'"),
    Some(SimplePattern("""file-(.+)\.csv""".r.pattern.pattern)),
    Some(parseExpression(
      """"#" ++ now(format="yyyy-MM-dd", timezone="Pacific/Tahiti") ++ "#F-$orderWatchPath:$1""""
    ).orThrow))

  private lazy val yyyymmdd = ZonedDateTime.now
    .withZoneSameInstant(ZoneId.of("Pacific/Tahiti"))
    .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))

  "relativePathToOrderId" in {
    assert(relativePathToOrderId(fileWatch, "X") == None)

    // Test may fail at midnight, Tahiti time, due to date change
    assert(relativePathToOrderId(fileWatch, "file-ORDER.csv") ==
      Some(Right(OrderId(s"#$yyyymmdd#F-FILE-WATCH:ORDER"))))
  }
}
