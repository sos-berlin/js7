package js7.data.value.expression.scopes

import cats.syntax.semigroup.*
import js7.base.test.Test
import js7.data.value.NumberValue
import js7.data.value.expression.Scope

final class CombinedScopeTest extends Test
{
  "Monoid combine" in {
    val aScope: Scope = NamedValueScope(
      "A" -> NumberValue(111))
    val bScope: Scope = NamedValueScope(
      "A" -> NumberValue(999),
      "B" -> NumberValue(222))
    val scope = aScope |+| bScope
    assert(scope.parseAndEval("$A") == Right(NumberValue(111)))
    assert(scope.parseAndEval("$B") == Right(NumberValue(222)))
  }
}
