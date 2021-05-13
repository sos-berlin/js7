package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.system.OperatingSystem.isJVM
import js7.data.value.StringValue
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

final class EnvScopeTest extends AnyFreeSpec
{
  if (isJVM) {
    "Env" - {
      "Known name" in {
        assert(EnvScope.parseAndEval(s"env('PATH')") == Right(StringValue(sys.env("PATH"))))
      }

      "Unknown name" in {
        val unknown = Random.nextString(32)
        assert(EnvScope.parseAndEval(s"env('$unknown')") ==
          Left(UnknownKeyProblem("environment variable", unknown)))
      }

      "Default constant" in {
        val unknown = Random.nextString(32)
        assert(EnvScope.parseAndEval(s"env('$unknown', 'DEFAULT')") == Right(StringValue("DEFAULT")))
      }

      "Default environment variable" in {
        val unknown = Random.nextString(32)
        val unknownDefault = Random.nextString(32)
        assert(EnvScope.parseAndEval(s"env('$unknown', env('$unknownDefault', 'DEFAULT'))") == Right(StringValue("DEFAULT")))
      }

      "Default parameter is lazy" in {
        val unknownDefault = Random.nextString(32)
        assert(EnvScope.parseAndEval(s"env('PATH', env('$unknownDefault'))") == Right(StringValue(sys.env("PATH"))))
      }
    }
  }
}
