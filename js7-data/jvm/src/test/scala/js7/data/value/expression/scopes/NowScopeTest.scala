package js7.data.value.expression.scopes

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, OffsetDateTime, ZoneId}
import js7.base.time.JavaTimestamp.specific._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.{NumberValue, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class NowScopeTest extends AnyFreeSpec
{
  private lazy val nowScope = new NowScope

  "Example with now() and $epochSecond" in {
    val checkedValue = nowScope.parseAndEval(
      "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \", $epochSecond\"")
    val yyyymmdd = LocalDateTime.ofInstant(nowScope.now.toInstant, ZoneId.of("Antarctica/Troll"))
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    val epochSecond = nowScope.now.toEpochMilli / 1000

    assert(checkedValue == Right(StringValue(s"#$yyyymmdd, $epochSecond")))
  }

  private val format = "yyyy-MM-dd HH:mm:SSZ"
  format in {
    val now = nowScope.now
    val checkedValue = nowScope.parseAndEval(s"now(format='$format')")
    val expected = OffsetDateTime.ofInstant(now.toInstant, ZoneId.systemDefault())
      .format(DateTimeFormatter.ofPattern(format))
    assert(checkedValue == Right(StringValue((expected))))
  }

  "$epochMilli" in {
    val scope = nowScope
    val epochMilli = nowScope.now.toEpochMilli
    val checkedValue = scope.parseAndEval("$epochMilli")
    assert(checkedValue == Right(NumberValue(epochMilli)))
    val n = checkedValue.flatMap(_.asNumber).orThrow.toLongExact
    assert(n == epochMilli)
  }
}
