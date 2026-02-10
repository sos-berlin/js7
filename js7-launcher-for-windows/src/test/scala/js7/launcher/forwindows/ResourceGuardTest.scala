package js7.launcher.forwindows

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.concurrent.ExecutionContext

final class ResourceGuardTest extends OurAsyncTestSuite:

  private given ExecutionContext = ioRuntime.compute

  "ResourceGuard immediately" in:
    var released = 0
    val g = ResourceGuard("RESOURCE") { _ => released += 1 }
    assert(g.use { o => o } == Some("RESOURCE"))
    assert(released == 0)

    g.releaseAfterUse()
    assert(released == 1)

    assert(g.use { o => o } == None)
    assert(released == 1)

    g.releaseAfterUse()
    assert(released == 1)

  "ResourceGuard delayed" in:
    var released = 0
    val g = ResourceGuard("RESOURCE") { _ => released += 1 }
    g.use:
      case Some("RESOURCE") =>
        g.releaseAfterUse()
        assert(released == 0)
      case _ =>
        fail()
    assert(released == 1)

  "ResourceGuard parallel" in:
    val released = Atomic(0)
    val g = ResourceGuard("RESOURCE") { _ => released += 1 }
    val notReleased = Atomic(0)

    val loopUntilReleased = IO:
      var stop = false
      while !stop do
        g.use:
          case Some("RESOURCE") => // not released
            assert(released.get() == 0)
            notReleased += 1
          case Some(_) =>
            fail()
          case None => // released
            stop = true

    val minimum = 100_000
    val n = sys.runtime.availableProcessors
    (1 to n).foldMap(_ => loopUntilReleased).both:
      IO.blocking:
        awaitAndAssert(notReleased.get() >= minimum)
        g.releaseAfterUse()
    .map:
      case ((), ()) =>
        assert(released.get() == 1)
        assert(notReleased.get() >= minimum)
