package js7.data.value.expression.scopes

import cats.syntax.semigroup._
import js7.data.value.NumberValue
import js7.data.value.expression.Scope
import org.scalatest.freespec.AnyFreeSpec

final class DoubleScopeTest extends AnyFreeSpec
{
  "Monoid combine" in {
    val aScope: Scope = new NamedValueScope(Map("A" -> NumberValue(111)))
    val bScope: Scope = new NamedValueScope(Map("A" -> NumberValue(999), "B" -> NumberValue(222)))
    val scope = aScope |+| bScope
    assert(scope.parseAndEval("$A") == Right(NumberValue(111)))
    assert(scope.parseAndEval("$B") == Right(NumberValue(222)))
  }
}
