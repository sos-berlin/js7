package com.sos.scheduler.engine.common.utils

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
    val t: Try[Int] = ignoreException(logger.debug) { 123 }
    assert(t == Success(123))
  }

  "ignoreException ignores exception" in {
    val exception = new RuntimeException("TEST")
    var ignored: Throwable = null
    def onException(message: ⇒ String, t: Throwable) = ignored = t
    val t = ignoreException(onException) {
      throw exception
    }
    assert(ignored eq exception)
    assert(t.isFailure)
    assert(t.failed.get.getMessage == "TEST")
  }

//  "ignoreException ignores exception, with slf4j.Logger.debug" in {
//    val slf4jLogger: slf4j.Logger = logger.delegate
//    ignoreException(slf4jLogger.debug) {
//      throw new RuntimeException
//    }
//  }

  "ignoreException ignores exception, with Logger.debug" in {
    ignoreException(logger.debug) {
      throw new RuntimeException("TEST")
    }
  }

  "toStringWithCauses" in {
    assert(toStringWithCauses(new RuntimeException("TEST")) == "java.lang.RuntimeException: TEST")
    assert(toStringWithCauses(new RuntimeException("TEST", new IllegalStateException("STATE"))) ==
      "java.lang.RuntimeException: TEST, caused by java.lang.IllegalStateException: STATE")
  }
}

object ExceptionsTest {
  private val logger = Logger(getClass)
}
