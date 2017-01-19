package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.Closers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import scala.collection.mutable
import scala.language.reflectiveCalls

@RunWith(classOf[JUnitRunner])
final class ClosersTest extends FreeSpec {

  "onClose" in {
    implicit val closer = Closer.create()
    var a = false
    closer.onClose { a = true }
    a shouldBe false
    closer.close()
    a shouldBe true
  }

  "registerAutoCloseable" in {
    implicit val closer = Closer.create()
    val autoCloseable = mock[AutoCloseable]
    closer.registerAutoCloseable(autoCloseable)
    verify(autoCloseable, never).close()
    closer.close()
    verify(autoCloseable).close()
  }

  "closeWithCloser AutoClosable" in {
    implicit val closer = Closer.create()
    val c = mock[AutoCloseable].closeWithCloser
    verify(c, never).close()
    closer.close()
    verify(c).close()
  }

  "AnyRef.withCloser" in {
    implicit val closer = Closer.create()
    trait A
    var closedA: A = null
    val c = mock[A].withCloser { a ⇒ closedA = a }
    assert(closedA == null)
    closer.close()
    assert(closedA == c)
  }

  "onCloseOrShutdownn" in {
    var closed = false
    val closer = Closer.create()
    closer.onCloseOrShutdown { closed = true }
    closed shouldBe false
    closer.close()
    closed shouldBe true
  }

  "withCloser" in {
    val a = mock[GuavaCloseable]
    withCloser { closer ⇒
      closer.register(a)
    }
    verify(a).close()
  }

  "closeOrdered" in {
    val closed = mutable.Buffer[Any]()
    val a = new AutoCloseable { def close() = closed += this }
    val b = new AutoCloseable { def close() = closed += this }
    closeOrdered(a, b)
    assert(closed == List(a, b))
  }

  "closeThen" in {
    val closes = mutable.Buffer[Int]()
    val closer = Closer.create()
    closer.onClose { closes += 1 }
    closer.closeThen { closes += 2 }
    assert(closes == List(1, 2))
  }
}
