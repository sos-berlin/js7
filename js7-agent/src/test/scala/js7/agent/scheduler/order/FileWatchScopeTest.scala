package js7.agent.scheduler.order

import cats.syntax.semigroup._
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.orderwatch.OrderWatchPath
import js7.data.value.StringValue
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.expression.scopes.NowScope
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchScopeTest extends AnyFreeSpec
{
  private val orderWatchPath = OrderWatchPath("FILE-WATCH")
  private val pattern = """file-(.*((A)|(B)))\.csv""".r.pattern
  private val filename = "file-100B.csv"

  private lazy val fileWatchScope = {
    val matcher = pattern.matcher(filename)
    val matches = matcher.matches()  // required !!!
    assert(matches)
    FileWatchScope(orderWatchPath, matcher)
  }

  "$0...$n" in {
    assert(fileWatchScope.parseAndEval("$0") == Right(StringValue("file-100B.csv")))
    assert(fileWatchScope.parseAndEval("\"$0\"") == Right(StringValue("file-100B.csv")))
    assert(fileWatchScope.parseAndEval("$1") == Right(StringValue("100B")))
    assert(fileWatchScope.parseAndEval("$2") == Right(StringValue("B")))
    assert(fileWatchScope.parseAndEval("$3") == Left(Problem("No such named value: 3")))
    assert(fileWatchScope.parseAndEval("$4") == Right(StringValue("B")))
    assert(fileWatchScope.parseAndEval("$5") == Left(Problem("No such named value: 5")))
  }

  "nameToCheckedValue" in {
    assert(fileWatchScope.nameToCheckedValue.toMap == Map(
      "0" -> Right(StringValue("file-100B.csv")),
      "1" -> Right(StringValue("100B")),
      "2" -> Right(StringValue("B")),
      "4" -> Right(StringValue("B")),
      "orderWatchPath" -> Right(StringValue("FILE-WATCH"))))
  }

  "Complete" in {
    implicit val scope = fileWatchScope |+| NowScope()
    val checkedValue = scope.parseAndEval(
      "'#' ++ now(format='yyyy-MM-dd', " +
        "timezone='Antarctica/Troll') ++ \"#F$epochSecond-$orderWatchPath:$1\"")
    val now = Instant.ofEpochSecond(NamedValue("epochSecond").evalAsNumber.orThrow.toLong)
    val yyyymmdd = LocalDateTime.ofInstant(now, ZoneId.of("Antarctica/Troll"))
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    val epochSecond = now.toEpochMilli / 1000

    assert(checkedValue == Right(StringValue(s"#$yyyymmdd#F$epochSecond-FILE-WATCH:100B")))
  }
}
