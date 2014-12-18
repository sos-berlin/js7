package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.HasCloserTest._
import com.sos.scheduler.engine.common.scalautil.HasCloser.implicits._
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import scala.language.reflectiveCalls

@RunWith(classOf[JUnitRunner])
final class HasCloserTest extends FreeSpec {

  "onClose" in {
    val a = new A
    a.closed shouldBe false
    a.close()
    a.closed shouldBe true
  }

  "implicit Closer.apply" in {
    import com.sos.scheduler.engine.common.scalautil.HasCloser.implicits._
    implicit val closer = Closer.create()
    var a = false
    closer { a = true }
    a shouldBe false
    closer.close()
    a shouldBe true
  }

  "registerCloseable" in {
    import com.sos.scheduler.engine.common.scalautil.HasCloser.implicits._
    implicit val closer = Closer.create()
    val c = mock[AutoCloseable].registerCloseable
    verify(c, times(0)).close()
    closer.close()
    verify(c, times(1)).close()
  }

  "registerAutoCloseable" in {
    val hasCloser = new HasCloser {
      val autoCloseable = mock[AutoCloseable]
      registerAutoCloseable(autoCloseable)
    }
    verify(hasCloser.autoCloseable, times(0)).close()
    hasCloser.close()
    verify(hasCloser.autoCloseable, times(1)).close()
  }

  "onCloseOrShutdownn" in {
    var closed = false
    val closer = Closer.create()
    closer.onCloseOrShutdown { closed = true }
    closed shouldBe false
    closer.close()
    closed shouldBe true
  }
}


private object HasCloserTest {

  private class A extends HasCloser {
    var closed = false
    onClose { closed = true }
  }
}
