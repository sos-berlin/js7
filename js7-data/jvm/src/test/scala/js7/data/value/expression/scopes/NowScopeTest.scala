package js7.data.value.expression.scopes

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.Timestamp
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.{NumberValue, StringValue}

final class NowScopeTest extends OurTestSuite:
  private lazy val nowScope = new NowScope(ts"2021-08-16T12:00:00Z")

  "Example with now() and $js7EpochSecond" in:
    val checkedValue = nowScope.parseAndEval(
      "'#' ++ now(format='yyyy-MM-dd', timezone='Antarctica/Troll') ++ \", $js7EpochSecond\"")
    val yyyymmdd = nowScope.now.toLocalDateTime(using ZoneId.of("Antarctica/Troll"))
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    val epochSecond = nowScope.now.toEpochMilli / 1000

    assert(checkedValue == Right(StringValue(s"#$yyyymmdd, $epochSecond")))

  private val format = "yyyy-MM-dd HH:mm:SSZ"
  format in:
    val now = nowScope.now
    val checkedValue = nowScope.parseAndEval(s"now(format='$format')")
    val expected = now.toOffsetDateTime(using ZoneId.systemDefault())
      .format(DateTimeFormatter.ofPattern(format))
    assert(checkedValue == Right(StringValue((expected))))

  "$js7EpochMilli" in:
    val scope = nowScope
    val epochMilli = nowScope.now.toEpochMilli
    val checkedValue = scope.parseAndEval("$js7EpochMilli")
    assert(checkedValue == Right(NumberValue(epochMilli)))
    val n = checkedValue.flatMap(_.asNumber).orThrow.toLongExact
    assert(n == epochMilli)

  "namedValue" in:
    assert(nowScope.namedValue("js7EpochMilli") == Some(Right(NumberValue(1629115200000L))))
    assert(nowScope.namedValue("js7EpochSecond") == Some(Right(NumberValue(1629115200))))
