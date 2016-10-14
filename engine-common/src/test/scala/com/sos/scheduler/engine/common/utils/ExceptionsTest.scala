package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.log.LazyScalaLogger.AsLazyScalaLogger
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.Exceptions._
import com.sos.scheduler.engine.common.utils.ExceptionsTest._
import java.io.IOException
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.util.{Success, Try}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ExceptionsTest extends FreeSpec {

  "repeatUntilNoException" in {
    var i = 0
    val t = Instant.now()
    repeatUntilNoException(10.s, 10.ms) {
      i += 1
      if (i < 5) sys.error("TEST")
    }
    assert(i == 5)
    val duration = Instant.now() - t
    assert(duration >= 40.ms && duration <= 200.ms)
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
    def onException(message: ⇒ String, t: Throwable) = ignored = (message, t)
    val t = ignoreException(onException) {
      throw exception
    }
    assert(ignored == ((s"Ignoring exception $exception", exception)))
    assert(t.isFailure)
    assert(t.failed.get.getMessage == "TEST")
  }

//  "ignoreException ignores exception, with slf4j.Logger.trace" in {
//    val slf4jLogger: slf4j.Logger = logger.delegate
//    ignoreException(slf4jLogger.trace) {
//      throw new RuntimeException
//    }
//  }

  "ignoreException ignores exception, with Logger" in {
    ignoreException(logger.asLazy.trace) {
      throw new RuntimeException("TEST")
    }
  }

  "logException" in {
    val exception = new RuntimeException("TEST")
    var logged: (String, Throwable) = null
    def onException(message: ⇒ String, t: Throwable) = logged = (message, t)
    intercept[RuntimeException] {
      logException(onException) {
        throw exception
      }
    }
    assert(logged == ((exception.toString, exception)))
  }
}

object ExceptionsTest {
  private val logger = Logger(getClass)
}
