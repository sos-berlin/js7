package js7.base.circeutils

import io.circe.syntax.EncoderOps
import java.util.regex.Pattern
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.circeutils.StandardJsonCodecs.{PatternJsonCodec, RegexJsonCodec}
import js7.base.problem.Problem
import js7.base.system.Java8Polyfill.javaVersion
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import scala.util.matching.Regex

final class StandardJsonCodecsTest extends OurTestSuite
{
  private val regexProblem = Problem(
    if (javaVersion >= 19)
      "JSON DecodingFailure at : java.util.regex.PatternSyntaxException: Unescaped trailing backslash near index 1\\n\\"
    else
      "JSON DecodingFailure at : java.util.regex.PatternSyntaxException: Unexpected internal error near index 1\\n\\")

  "Pattern" in {
    // Pattern does not implement `equals`
    val json = json"""".*""""
    assert(".*".r.pattern.asJson == json)
    assert(json.as[Pattern].toChecked.orThrow.pattern == ".*".r.pattern.pattern)
    assert("\\".asJson.as[Pattern].toChecked == Left(regexProblem))
  }

  "Regex" in {
    // Regex does not implement `equals`
    val json = json"""".*""""
    assert(".*".r.asJson == json)
    assert(json.as[Regex].toChecked.orThrow.pattern.pattern == ".*".r.pattern.pattern)
    assert("\\".asJson.as[Regex].toChecked == Left(regexProblem))
  }
}
