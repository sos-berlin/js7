package js7.base.test

import cats.syntax.monoid.*
import cats.syntax.foldable.*
import org.scalatest.freespec.AnyFreeSpec
import ScalaTestForCats.given
import org.scalatest.FixtureContext

final class ScalaTestForCatsTest extends AnyFreeSpec:

  "Monoid[Assertion]" in:
    assert(Seq(succeed, succeed, succeed).combineAll == succeed)

    val e = intercept[IllegalArgumentException]:
      Seq(succeed, new FixtureContext {}).combineAll
    assert(e.getMessage.startsWith("Unexpected Assertion value: "))  
