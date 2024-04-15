package js7.base.test

import cats.syntax.foldable.*
import js7.base.test.ScalaTestForCats.given
import org.scalatest.FixtureContext
import org.scalatest.freespec.AnyFreeSpec

final class ScalaTestForCatsTest extends AnyFreeSpec:

  "Monoid[Assertion]" in:
    assert(Seq(succeed, succeed, succeed).combineAll == succeed)

    val e = intercept[IllegalArgumentException]:
      Seq(succeed, new FixtureContext {}).combineAll
    assert(e.getMessage.startsWith("Unexpected Assertion value: "))
