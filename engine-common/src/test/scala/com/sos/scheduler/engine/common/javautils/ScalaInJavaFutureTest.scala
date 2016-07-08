package com.sos.scheduler.engine.common.javautils

import java.util.concurrent.TimeUnit.MILLISECONDS
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ScalaInJavaFutureTest extends FreeSpec {

  "isDone and get" in {
    val promise = Promise[Int]()
    val future: java.util.concurrent.Future[Int] = new ScalaInJavaFuture(promise.future)
    assert(!future.isDone)
    for (t ← List(0, 1)) {
      intercept[java.util.concurrent.TimeoutException] {
        future.get(t, MILLISECONDS)
      }
    }
    promise.success(7)
    assert(future.isDone)
    assert(future.get() == 7)
  }

  "failure" in {
    val promise = Promise[Int]()
    val future: java.util.concurrent.Future[Int] = new ScalaInJavaFuture(promise.future)
    assert(!future.isDone)
    promise.failure(new Exception("TEST-ERROR"))
    assert(future.isDone)
    intercept[java.util.concurrent.ExecutionException] {
      future.get()
    } .getMessage should include ("TEST-ERROR")
  }
}
