package js7.base.test

import cats.Monoid
import js7.base.log.Logger
import js7.base.test.TestMixin.assertionMonoid
import org.scalatest.{Assertion, Succeeded, Suite}

trait TestMixin extends TestCatsEffect:
  this: Suite =>

  /** For use as AdHocLogger. */
  protected final val Logger = js7.base.log.Logger

  protected final given Monoid[Assertion] = assertionMonoid


object TestMixin:

  /** Only for Succeeded value, not for FixtureContext. */
  given assertionMonoid: Monoid[Assertion] =
    new Monoid[Assertion]:
      def empty = Succeeded

      def combine(a: Assertion, b: Assertion) =
        assert(a == Succeeded && b == Succeeded)
        Succeeded
