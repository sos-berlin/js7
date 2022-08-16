package js7.base.utils

import cats.effect.{Resource, SyncIO}
import js7.base.test.Test
import js7.base.utils.SyncResource.syntax.*
import monix.execution.atomic.AtomicAny
import scala.collection.mutable
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class SyncResourceTest extends Test
{
  "SyncIO useSync" in {
    val opened = mutable.Set.empty[MySimpleResource]

    class MySimpleResource extends AutoCloseable {
      private val value = AtomicAny[java.lang.Long](Random.nextLong())
      opened += this

      def isOpened = value.get() != null

      def close() = {
        assert(value.getAndSet(null) != null)
        opened -= this
      }
    }

    val resource = Resource.fromAutoCloseable(SyncIO { new MySimpleResource })
    assert(opened.isEmpty)
    val b = resource.useSync { a =>
      assert(a.isOpened)
      assert(opened == Set(a))
      7
    }
    assert(b == 7 && opened.isEmpty)
  }
}
