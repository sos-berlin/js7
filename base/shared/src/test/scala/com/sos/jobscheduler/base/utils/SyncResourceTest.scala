package com.sos.jobscheduler.base.utils

import cats.effect.{Resource, SyncIO}
import com.sos.jobscheduler.base.utils.SyncResource.syntax._
import monix.execution.atomic.AtomicAny
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class SyncResourceTest extends FreeSpec
{
  "SyncIO useSync" in {
    var opened = mutable.Set[MySimpleResource]()

    class MySimpleResource extends AutoCloseable {
      private val value = AtomicAny[java.lang.Long](Random.nextLong())
      opened += this

      def isOpened = value.get != null

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
