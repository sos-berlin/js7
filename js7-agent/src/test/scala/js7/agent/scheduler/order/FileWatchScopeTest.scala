package js7.agent.scheduler.order

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.orderwatch.OrderWatchId
import js7.data.value.{NumberValue, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchScopeTest extends AnyFreeSpec
{
  private val orderWatchId = OrderWatchId("FILE-WATCH")
  private val pattern = """file-(.+)\.csv""".r.pattern
  private val filename = "file-1000.csv"

  private def newFileWatchScope(string: String) = {
    val matcher = pattern.matcher(string)
    val matches = matcher.matches()  // required !!!
    assert(matches)
    new FileWatchScope(orderWatchId, matcher)
  }

  "Example with now() and $epochSecond" in {
    val fileWatchScope = newFileWatchScope(filename)
    val checkedValue = fileWatchScope.eval(
      "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \"#F$epochSecond-$orderWatchId:$1\"")
    val yyyymmdd = LocalDateTime.ofInstant(fileWatchScope.now, ZoneId.of("Antarctica/Troll"))
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    val epochSecond = fileWatchScope.now.toEpochMilli / 1000

    assert(checkedValue == Right(StringValue(s"#$yyyymmdd#F$epochSecond-FILE-WATCH:1000")))
  }

  "$epochMilli" in {
    val now1 = Instant.now.toEpochMilli
    val fileWatchScope = newFileWatchScope(filename)
    val epochMilli = fileWatchScope.now.toEpochMilli
    val checkedValue = fileWatchScope.eval("$epochMilli")
    val now2 = Instant.now.toEpochMilli
    assert(checkedValue == Right(NumberValue(epochMilli)))
    val n = checkedValue.flatMap(_.toNumber).orThrow.toBigDecimal.longValue
    assert(now1 <= n && n <= now2)
  }
}
