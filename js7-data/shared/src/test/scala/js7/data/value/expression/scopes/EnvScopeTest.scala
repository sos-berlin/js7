package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.system.OperatingSystem.isJVM
import js7.base.test.Test
import js7.data.value.StringValue
import scala.PartialFunction.condOpt
import scala.util.Random

final class EnvScopeTest extends Test
{
  if (isJVM) {
    "Env" - {
      "Known name" in {
        assert(EnvScope.parseAndEval(s"env('PATH')") == Right(StringValue(sys.env("PATH"))))
      }

      "Unknown name" in {
        val unknown = randomString()
        assert(EnvScope.parseAndEval(s"env('$unknown')") ==
          Left(UnknownKeyProblem("environment variable", unknown)))
      }

      "Default constant" in {
        val unknown = randomString()
        assert(EnvScope.parseAndEval(s"env('$unknown', 'DEFAULT')") == Right(StringValue("DEFAULT")))
      }

      "Default environment variable" in {
        val unknown = randomString()
        val unknownDefault = randomString()
        assert(EnvScope.parseAndEval(s"env('$unknown', env('$unknownDefault', 'DEFAULT'))") == Right(StringValue("DEFAULT")))
      }

      "Default parameter is lazy" in {
        val unknownDefault = randomString()
        assert(EnvScope.parseAndEval(s"env('PATH', env('$unknownDefault'))") == Right(StringValue(sys.env("PATH"))))
      }

      "Nested call" in {
        val envScope = new EnvScope {
          def get(name: String) =
            condOpt(name) {
              case "A" => "B"
              case "B" => "RESULT"
            }
        }
        assert(envScope.parseAndEval("env(env('A'))") == Right(StringValue("RESULT")))
      }
    }
  }

  private def randomString() = new String(Array.fill(32)(('a' + Random.nextInt(26)).toChar))
}
