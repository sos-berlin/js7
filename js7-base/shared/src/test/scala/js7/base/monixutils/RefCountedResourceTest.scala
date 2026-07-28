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

    locally:
      for
        _ = assert(count.get() == 0)
        a <- refCountedResource.resource
        _ = assert(a == 1 && count.get() == 1)
        b <- refCountedResource.resource
        _ = assert(b == 1 && count.get() == 1)
      yield
        a
    .use_ *>
      refCountedResource.release.map: _ =>
        assert(count.get() == 0)
