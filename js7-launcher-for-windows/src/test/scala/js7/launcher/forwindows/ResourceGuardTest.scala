package js7.launcher.forwindows

import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

final class ResourceGuardTest extends OurTestSuite
{
  "ResourceGuard immediately" in {
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
  }

  "ResourceGuard delayed" in {
    var released = 0
    val g = ResourceGuard("RESOURCE") { _ => released += 1 }
    g.use {
      case Some("RESOURCE") =>
        g.releaseAfterUse()
        assert(released == 0)
      case _ =>
        fail()
    }
    assert(released == 1)
  }

  "ResourceGuard parallel" in {
    var released = 0
    val g = ResourceGuard("RESOURCE") { _ => released += 1 }
    var notReleased = 0
    val futures = for (_ <- 1 to (Runtime.getRuntime.availableProcessors / 2).min(1)) yield
      Future {
        var stop = false
        while (!stop) {
          g.use {
            case Some("RESOURCE") =>
              assert(released == 0)
              notReleased += 1
            case Some(_) =>
              fail()
            case None =>
              stop = true
          }
        }
      }
    sleep(100.ms)
    g.releaseAfterUse()
    futures await 99.s
    assert(released == 1)
    assert(notReleased > 1)
  }
}
