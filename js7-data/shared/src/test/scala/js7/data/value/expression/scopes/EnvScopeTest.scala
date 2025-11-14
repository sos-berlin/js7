package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.system.OperatingSystem.{PathEnvName, isJVM}
import js7.base.test.OurTestSuite
import js7.data.value.StringValue
import scala.PartialFunction.condOpt
import scala.util.Random

final class EnvScopeTest extends OurTestSuite:

  if isJVM then
    "Env" - {
      "Known name" in:
        assert(EnvScope.parseAndEval(s"env('$PathEnvName')") ==
          Right(StringValue(sys.env(PathEnvName))))

      "Unknown name" in:
        val unknown = randomString()
        assert(EnvScope.parseAndEval(s"env('$unknown')") ==
          Left(UnknownKeyProblem("environment variable", unknown)))

      "Default constant" in:
        val unknown = randomString()
        assert(EnvScope.parseAndEval(s"env('$unknown', 'DEFAULT')") == Right(StringValue("DEFAULT")))

      "Default environment variable" in:
        val unknown = randomString()
        val unknownDefault = randomString()
        assert(EnvScope.parseAndEval(s"env('$unknown', env('$unknownDefault', 'DEFAULT'))") ==
          Right(StringValue("DEFAULT")))

      "Default parameter is lazy" in:
        val unknownDefault = randomString()
        assert(EnvScope.parseAndEval(s"env('$PathEnvName', env('$unknownDefault'))") ==
          Right(StringValue(sys.env(PathEnvName))))

      "Nested call" in:
        val envScope = new EnvScope:
          override def getEnv(name: String) =
            condOpt(name):
              case "A" => "B"
              case "B" => "RESULT"
        assert(envScope.parseAndEval("env(env('A'))") == Right(StringValue("RESULT")))
    }

  private def randomString() = new String(Array.fill(32)(('a' + Random.nextInt(26)).toChar))
