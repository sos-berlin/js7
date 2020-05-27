package com.sos.jobscheduler.base.web

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.web.HttpClient.liftProblem
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec
import scala.util.Failure

/**
  * @author Joacim Zschimmer
  */
final class HttpClientTest extends AsyncFreeSpec
{
  private val problem = Problem.pure("PROBLEM")
  private val withProblem = new HttpClient.HttpException() {
    def statusInt = 400
    val problem = Some(HttpClientTest.this.problem)
  }
  private val withoutProblem = new HttpClient.HttpException() {
    def statusInt = 400
    val problem = None
  }

  "HasProblem" in {
    assert(withProblem match {
      case HttpClient.HttpException.HasProblem(`problem`) => true
      case _ => false
    })
    assert(withoutProblem match {
      case HttpClient.HttpException.HasProblem(`problem`) => false
      case _ => true
    })
  }

  "liftProblem withProblem" in {
    for (checkedProblem <- liftProblem(Task.raiseError[Int](withProblem)).runToFuture) yield
      assert(checkedProblem == Left(problem))
  }

  "liftProblem withoutProblem" in {
    for (tried <- liftProblem(Task.raiseError[Int](withoutProblem)).materialize.runToFuture) yield
      assert(tried == Failure(withoutProblem))
  }
}
