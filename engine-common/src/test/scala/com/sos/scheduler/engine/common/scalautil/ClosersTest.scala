package com.sos.scheduler.engine.common.scalautil

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
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
    verify(autoCloseable, times(0)).close()
    closer.close()
    verify(autoCloseable, times(1)).close()
  }

  "registerCloseable AutoClosable" in {
    implicit val closer = Closer.create()
    val c = mock[AutoCloseable].closeWithCloser
    verify(c, times(0)).close()
    closer.close()
    verify(c, times(1)).close()
  }

  "registerCloseable with some close" in {
    implicit val closer = Closer.create()
    trait A { def close() }
    val c = mock[A].closeWithCloser
    verify(c, times(0)).close()
    closer.close()
    verify(c, times(1)).close()
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
