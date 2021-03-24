package js7.agent.scheduler.order

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import js7.agent.scheduler.order.FileWatchManager.relativePathToOrderId
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.SimplePattern
import js7.data.agent.AgentId
import js7.data.order.OrderId
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.value.expression.ExpressionParser
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchManagerTest extends AnyFreeSpec
{
  private lazy val fileWatch = FileWatch(
    OrderWatchId("FILE-WATCH"),
    WorkflowPath("WORKFLOW"),
    AgentId("AGENT"),
    "DIRECTORY",
    Some(SimplePattern("""file-(.+)\.csv""".r.pattern.pattern)),
    Some(ExpressionParser.parse(
      """"#" ++ now(format="yyyy-MM-dd", timezone="Pacific/Tahiti") ++ "#F-$orderWatchId:$1""""
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
