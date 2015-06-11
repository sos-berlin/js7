package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, OneInstancePerTest}

@RunWith(classOf[JUnitRunner])
final class StandardCallQueueTest extends FunSuite with OneInstancePerTest {
  private val callQueue = new StandardCallQueue

  test("warm-up") {
    callQueue {}
    callQueue add TimedCall(now() + 50.ms) {}
    callQueue.popMature()
  }

  test("add(=>A)") {
    var a = 0
    callQueue { a += 1 }
    val call = callQueue.popMature().get
    call.epochMillis should equal (0)
    call.onApply()
    a should equal (1)
    callQueue.popMature() should equal (None)
  }

  test("add(short_term, TimedCall)") {
    var a = 0
    val call = TimedCall(TimedCall.ShortTerm) { a += 1 }
    callQueue add call
    val c = callQueue.popMature().get
    c should be theSameInstanceAs call
    c.onApply()
    a should equal (1)
    callQueue.popMature() should equal (None)
  }

  test("add(at, TimedCall)") {
    var a = 0
    val call = TimedCall(now() + 500.ms) { a += 1 }
    callQueue.add(call)
    callQueue.popMature() should equal (None)
    sleep(100.ms)
    callQueue.popMature() should equal (None)
    sleep(420.ms)
    val c = callQueue.popMature().get
    c should be theSameInstanceAs call
    c.onApply()
    a should equal (1)
    callQueue.popMature() should equal (None)
  }

  test("remove") {
    val call = TimedCall(now() + 500.ms) {}
    callQueue.add(call)
    callQueue.remove(call)
    callQueue.tryCancel(call) should equal (false)
    callQueue should be ('empty)
    intercept[RuntimeException] { callQueue.remove(call) }
  }
}
