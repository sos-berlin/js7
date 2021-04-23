package js7.data.value.expression.scopes

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, OffsetDateTime, ZoneId}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.{NumberValue, StringValue}
import org.scalatest.freespec.AnyFreeSpec

final class NowScopeTest extends AnyFreeSpec
{
  private lazy val nowScope = new NowScope

  "Example with now() and $epochSecond" in {
    val checkedValue = nowScope.parseAndEval(
      "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \", $epochSecond\"")
    val yyyymmdd = LocalDateTime.ofInstant(nowScope.now, ZoneId.of("Antarctica/Troll"))
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    val epochSecond = nowScope.now.toEpochMilli / 1000

    assert(checkedValue == Right(StringValue(s"#$yyyymmdd, $epochSecond")))
  }

  val format = "yyyy-MM-dd HH:mm:SSZ"
  format in {
    val now = nowScope.now
    val checkedValue = nowScope.parseAndEval(s"now(format='$format')")
    val expected = OffsetDateTime.ofInstant(now, ZoneId.systemDefault())
      .format(DateTimeFormatter.ofPattern(format))
    assert(checkedValue == Right(StringValue((expected))))
  }

  "$epochMilli" in {
    val epochMilli = nowScope.now.toEpochMilli
    val checkedValue = nowScope.parseAndEval("$epochMilli")
    assert(checkedValue == Right(NumberValue(epochMilli)))
    val n = checkedValue.flatMap(_.toNumber).orThrow.toBigDecimal.longValue
    assert(n == epochMilli)
  }
}
