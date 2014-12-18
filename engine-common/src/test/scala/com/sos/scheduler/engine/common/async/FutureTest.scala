package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.common.async.FutureTest._
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, OneInstancePerTest}
import scala.collection.mutable
import scala.concurrent._
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class FutureTest extends FunSuite with OneInstancePerTest {
  implicit private val executionContext = new QueuedExecutionContextExecutor

  test("Success") {
    val f = Future { "Hallo!" }
    f.isCompleted should be (false)
    executionContext.run()
    f.isCompleted should be (true)
    f.value should equal (Some(Success("Hallo!")))
  }

  test("Failure") {
    val f = Future { throw TestException() }
    f.isCompleted should be (false)
    executionContext.run()
    f.isCompleted should be (true)
    intercept[TestException] { f.value.get.get }
  }
}

private object FutureTest {
  private case class TestException() extends RuntimeException

  private class QueuedExecutionContextExecutor extends ExecutionContextExecutor {
    private val queue = mutable.UnrolledBuffer[Runnable]()

    def execute(runnable: Runnable): Unit = {
      queue += runnable
    }

    def run(): Unit = {
      queue foreach { r => r.run() }
      queue.remove(0, queue.size)
    }

    def reportFailure(t: Throwable): Unit = {
      sys.error(t.toString)
    }
  }
}
