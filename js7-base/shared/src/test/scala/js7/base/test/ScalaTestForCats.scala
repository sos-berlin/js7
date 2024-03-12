package js7.base.test

import cats.kernel.Monoid
import org.scalatest.{Assertion, Succeeded}

object ScalaTestForCats:

  /** There is only one valid value for Assertion: Succeeded.
   *
   * A failure is not a value but throws an exception.
   *
   * Allows Seq[Assertion](...).combineAll
   */
  given Monoid[Assertion] =
    new Monoid[Assertion]:
      val empty = Succeeded

      def combine(a: Assertion, b: Assertion) =
        (a, b) match
          case (Succeeded, Succeeded) => Succeeded
          case other => throw new IllegalArgumentException(s"Unexpected Assertion value: $other")
