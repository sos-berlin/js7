package js7.agent.scheduler.order

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.orderwatch.OrderWatchPath
import js7.data.value.StringValue
import js7.data.value.expression.Expression.NamedValue
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchScopeTest extends AnyFreeSpec
{
  private val orderWatchPath = OrderWatchPath("FILE-WATCH")
  private val pattern = """file-(.*((A)|(B)))\.csv""".r.pattern
  private val filename = "file-100B.csv"

  private lazy val scope = {
    val matcher = pattern.matcher(filename)
    val matches = matcher.matches()  // required !!!
    assert(matches)
    FileWatchScope(orderWatchPath, matcher)
  }

  "$0...$n" in {
    assert(scope.parseAndEval("$0") == Right(StringValue("file-100B.csv")))
    assert(scope.parseAndEval("\"$0\"") == Right(StringValue("file-100B.csv")))
    assert(scope.parseAndEval("$1") == Right(StringValue("100B")))
    assert(scope.parseAndEval("$2") == Right(StringValue("B")))
    assert(scope.parseAndEval("$3") == Left(Problem("No such named value: 3")))
    assert(scope.parseAndEval("$4") == Right(StringValue("B")))
    assert(scope.parseAndEval("$5") == Left(Problem("No such named value: 5")))
  }

  "Complete" in {
    val checkedValue = scope.parseAndEval(
      "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$epochSecond-$orderWatchPath:$1\"")
    val now = Instant.ofEpochSecond(scope.evalString(NamedValue("epochSecond")).orThrow.toLong)
    val yyyymmdd = LocalDateTime.ofInstant(now, ZoneId.of("Antarctica/Troll"))
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    val epochSecond = now.toEpochMilli / 1000

    assert(checkedValue == Right(StringValue(s"#$yyyymmdd#F$epochSecond-FILE-WATCH:100B")))
  }
}
