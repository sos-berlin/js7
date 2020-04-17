package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.Exceptions._
import com.sos.jobscheduler.common.utils.ExceptionsTest._
import java.io.IOException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.concurrent.duration.Deadline.now
import scala.util.{Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class ExceptionsTest extends AnyFreeSpec {

  "repeatUntilNoException" in {
    // Warm-up
    var _i = 0
    repeatUntilNoException(10.s, 10.ms) { _i += 1;if (_i < 2) sys.error("TEST") }

    val runningSince = now
    var i = 0
    repeatUntilNoException(10.s, 100.ms) {
      i += 1
      if (i < 5) sys.error("TEST")
    }
    assert(i == 5)
    val duration = runningSince.elapsed
    assert(duration >= 400.ms && duration <= 7.s)
    intercept[IOException] {
      repeatUntilNoException(100.ms, 10.ms) { throw new IOException }
    }
  }

  "ignoreException executes code" in {
    val t: Try[Int] = ignoreException(logger.asLazy.trace) { 123 }
    assert(t == Success(123))
  }

  "ignoreException ignores exception" in {
    val exception = new RuntimeException("TEST")
    var ignored: (String, Throwable) = null
    def onException(message: => String, t: Throwable) = ignored = (message, t)
    val t = ignoreException(onException) {
      throw exception
    }
    assert(ignored == ((s"Ignoring exception $exception", exception)))
    assert(t.isFailure)
    assert(t.failed.get.getMessage == "TEST")
  }

  "ignoreException ignores exception, with Logger" in {
    ignoreException(logger.asLazy.trace) {
      throw new RuntimeException("TEST")
    }
  }

  "logException" in {
    val exception = new RuntimeException("TEST")
    var logged: (String, Throwable) = null
    def onException(message: => String, t: Throwable) = logged = (message, t)
    intercept[RuntimeException] {
      logException(onException) {
        throw exception
      }
    }
    assert(logged == ((exception.toString, exception)))
  }

  "andRethrow" in {
    intercept[IllegalArgumentException] {
      try throw new IllegalArgumentException
      catch andRethrow {}
    } .getSuppressed shouldEqual Array()
    intercept[IllegalArgumentException] {
      try throw new IllegalArgumentException
      catch andRethrow { throw new IllegalStateException }
    } .getSuppressed map { _.getClass } shouldBe Array(classOf[IllegalStateException])
  }
}

object ExceptionsTest {
  private val logger = Logger(getClass)
}
