package js7.base.monixutils

import cats.effect.Resource
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.syntax.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced

final class RefCountedResourceTest extends OurAsyncTestSuite:
  "RefCountedResource" in:
    val count = Atomic(0)
    val baseResource = Resource.make(Task(count.incrementAndGet()))(_ => Task(count -= 1))
    val refCountedResource = new RefCountedResource(baseResource)
    val resource = refCountedResource.resource
    assert(count.get() == 0)

    val check =
      resource.allocated
        .flatMap { case (a, releaseA) =>
          assert(count.get() == 1 && a == 1)
          resource.allocated
            .flatMap { case (b, releaseB) =>
              assert(count.get() == 1 && b == 1)
              releaseB.map(_ => assert(count.get() == 1))
                .flatMap(_ =>
                  releaseB.map(_ => assert(count.get() == 1))
                    .flatMap(_ => releaseA.map(_ => assert(count.get() == 1))))
            }
        }
        .flatMap(_ =>
          refCountedResource.release
            .map(_ => assert(count.get() == 0)))

    check
      .runToFuture
