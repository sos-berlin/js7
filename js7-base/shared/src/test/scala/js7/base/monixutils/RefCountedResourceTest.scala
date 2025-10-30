package js7.base.monixutils

import cats.effect.{IO, Resource}
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*

final class RefCountedResourceTest extends OurAsyncTestSuite:

  "RefCountedResource" in:
    val count = Atomic(0)
    val baseResource = Resource.make(IO(count.incrementAndGet()))(_ => IO(count -= 1))
    val refCountedResource = RefCountedResource(baseResource)
    val resource = refCountedResource.resource
    assert(count.get() == 0)

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
