package js7.base.monixutils

import cats.effect.Resource
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.atomic.Atomic
import js7.base.test.OurAsyncTestSuite

final class RefCountedResourceTest extends OurAsyncTestSuite
{
  "RefCountedResource" in {
    val count = Atomic(0)
    val baseResource = Resource.make(Task(count.incrementAndGet()))(_ => Task(count -= 1))
    val refCountedResource = new RefCountedResource(baseResource)
    val resource = refCountedResource.resource
    assert(count() == 0)

    val check =
      resource.allocated
        .flatMap { case (a, releaseA) =>
          assert(count() == 1 && a == 1)
          resource.allocated
            .flatMap { case (b, releaseB) =>
              assert(count() == 1 && b == 1)
              releaseB.map(_ => assert(count() == 1))
                .flatMap(_ =>
                  releaseB.map(_ => assert(count() == 1))
                    .flatMap(_ => releaseA.map(_ => assert(count() == 1))))
            }
        }
        .flatMap(_ =>
          refCountedResource.release
            .map(_ => assert(count() == 0)))

    check
      .runToFuture
  }
}
