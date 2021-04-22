package js7.data.value.expression

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.system.OperatingSystem.{isJVM, isWindows}
import js7.data.value.StringValue
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

final class ScopeTest extends AnyFreeSpec
{
  if (isJVM) {
    "Env" - {
      "Known name" in {
        val path = if (isWindows) "Path" else "PATH"
        assert(Scope.Env.parseAndEval(s"env('$path')") == Right(StringValue(sys.env(path))))
      }

      "Unknown name" in {
        val unknown = Random.nextString(32)
        assert(Scope.Env.parseAndEval(s"env('$unknown')") ==
          Left(UnknownKeyProblem("environment variable", unknown)))
      }
    }
  }
}
