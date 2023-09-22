package js7.base.utils

import cats.effect.{ContextShift, IO, Resource}
import js7.base.catsutils.UnsafeMemoizable.concurrentMemoizable
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.CatsUtils.syntax.RichResource
import monix.execution.Scheduler

final class AllocatedTest extends OurAsyncTestSuite {

  private var nr = 0
  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(Scheduler.traced)

  private val resource = Resource.make(
    acquire = IO {
      nr += 1
      nr
    })(
    release = o => IO {
      assert(o == nr)
      nr -= 1
      assert(nr >= 0)
    })

  "Normal usage" in {
    val res = resource.flatMap(a => resource.map(a -> _))
    res.use { case (a, b) =>
      IO {
        assert(nr == 2 && a == 1 && b == 2)
      }
    }.unsafeToFuture()
  }

  "Allocated" in {
    assert(nr == 0)
    val res = resource.toAllocatedResource.flatMap(a => resource.toAllocatedResource.map(a -> _))
    res.use { case (a, b) =>
      IO {
        assert(nr == 2 && a.allocatedThing == 1 && b.allocatedThing == 2)
      }
    }.map(_ => assert(nr == 0))
      .unsafeToFuture()
  }
}
