package js7.base.test

import cats.Monoid
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.metering.CallMeter
import js7.base.test.TestMixin.assertionMonoid
import js7.base.utils.Tests.isIntelliJIdea
import org.scalatest.{Assertion, Informer, Succeeded, Suite}

trait TestMixin extends TestCatsEffect:
  this: Suite =>

  /** For use as AdHocLogger. */
  protected final inline def Logger = js7.base.log.Logger

  protected final given Monoid[Assertion] = assertionMonoid

  private var stopCallMeterService: IO[Unit] = IO.unit

  private given IORuntime = ioRuntime

  override protected def beforeAll(): Unit =
    super.beforeAll()
    if isIntelliJIdea then
      stopCallMeterService = CallMeter.loggingService(CallMeter.Conf.forTesting)
        .map(_ => ()).allocated.map(_._2).unsafeRunSync()

  override protected def afterAll() =
    stopCallMeterService.unsafeRunSync()
    super.afterAll()

  /** Usable to check the type of something. */
  inline final def expectType[A](inline something: A): Unit = ()

  // Defined in AnyFreeSpecLike and AsyncFreeSpecLike
  protected def info: Informer

  protected final def missingTest: Unit =
    info("❌ TEST IS MISSING")
    pending


object TestMixin:

  /** Only for Succeeded value, fails for FixtureContext. */
  given assertionMonoid: Monoid[Assertion] =
    new Monoid[Assertion]:
      def empty = Succeeded

      def combine(a: Assertion, b: Assertion) =
        assert(a == Succeeded && b == Succeeded)
        Succeeded
