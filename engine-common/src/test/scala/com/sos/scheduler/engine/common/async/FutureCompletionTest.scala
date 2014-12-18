package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.async.FutureCompletion.{futureCall, futureTimedCall, callFuture}
import com.sos.scheduler.engine.common.time.ScalaJoda._
import org.joda.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FreeSpec, OneInstancePerTest, FunSuite}
import scala.util.Success

@RunWith(classOf[JUnitRunner])
final class FutureCompletionTest extends FreeSpec with OneInstancePerTest {

  private lazy val queue: PoppableCallQueue = new StandardCallQueue
  private lazy val dispatcher = new CallRunner(queue)

  "warm-up" in {
    val call = futureTimedCall(now()) {}
    queue.add(call)
    val future = call.future
    dispatcher.executeMatureCalls()
    future.isCompleted
  }

  "Success" in {
    val call = futureTimedCall(now() + 500.ms) { "Hej!" }
    queue.add(call)
    val future = call.future
    (future.isCompleted, future.value) shouldBe (false, None)
    dispatcher.executeMatureCalls()
    (future.isCompleted, future.value) shouldBe (false, None)
    sleep(520.ms)
    dispatcher.executeMatureCalls()
    (future.isCompleted, future.value) shouldBe (true, Some(Success("Hej!")))
  }

  "Failure" in {
    val call = futureTimedCall(now() + 500.ms) { throw new TestException }
    queue.add(call)
    val future = call.future
    (future.isCompleted, future.value) shouldBe (false, None)
    dispatcher.executeMatureCalls()
    (future.isCompleted, future.value) shouldBe (false, None)
    sleep(501.ms)
    dispatcher.executeMatureCalls()
    future.isCompleted shouldBe true
    intercept[TestException] { future.value.get.get }
  }

  "ShortTermCall Success" in {
    val call = futureCall { "Hej!" }
    queue.add(call)
    val future = call.future
    (future.isCompleted, future.value) shouldBe (false, None)
    dispatcher.executeMatureCalls()
    (future.isCompleted, future.value) shouldBe (true, Some(Success("Hej!")))
  }

  "ShortTermCall Failure" in {
    val call = futureCall { throw new TestException }
    queue.add(call)
    val future = call.future
    (future.isCompleted, future.value) shouldBe (false, None)
    dispatcher.executeMatureCalls()
    future.isCompleted shouldBe true
    intercept[TestException] { future.value.get.get }
  }

  "callFuture" in {
    implicit val implicitQueue = queue
    val future = callFuture { throw new TestException }
    (future.isCompleted, future.value) shouldBe (false, None)
    dispatcher.executeMatureCalls()
    future.isCompleted shouldBe true
    intercept[TestException] { future.value.get.get }
  }

  private case class TestException() extends RuntimeException
}
