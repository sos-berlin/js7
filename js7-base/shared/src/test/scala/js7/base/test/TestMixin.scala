package js7.base.test

import cats.Monoid
import js7.base.test.TestMixin.assertionMonoid
import org.scalatest.{Assertion, Succeeded, Suite}

trait TestMixin extends TestCatsEffect:
  this: Suite =>

  /** For use as AdHocLogger. */
  protected final val Logger = js7.base.log.Logger

  protected final given Monoid[Assertion] = assertionMonoid

  /** Usable to check the type of something. */
  inline final def expectType[A](inline something: A): Unit = ()


object TestMixin:

  /** Only for Succeeded value, fails for FixtureContext. */
  given assertionMonoid: Monoid[Assertion] =
    new Monoid[Assertion]:
      def empty = Succeeded

      def combine(a: Assertion, b: Assertion) =
        assert(a == Succeeded && b == Succeeded)
        Succeeded
