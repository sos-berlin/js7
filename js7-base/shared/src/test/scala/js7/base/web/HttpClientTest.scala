package js7.base.web

import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite
import js7.base.web.HttpClient.liftProblem
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class HttpClientTest extends OurAsyncTestSuite:
  private val problem = Problem.pure("PROBLEM")
  private val withProblem = new HttpClient.HttpException:
    def statusInt = 503
    val problem = Some(HttpClientTest.this.problem)
  private val withoutProblem = new HttpClient.HttpException:
    def statusInt = 503
    val problem = None
    override def getMessage = "WITHOUT PROBLEM"

  "HasProblem" in:
    assert(withProblem match {
      case HttpClient.HttpException.HasProblem(`problem`) => true
      case _ => false
    })
    assert(withoutProblem match {
      case HttpClient.HttpException.HasProblem(`problem`) => false
      case _ => true
    })

  "liftProblem withProblem" in:
    for checkedProblem <- liftProblem(Task.raiseError[Int](withProblem)).runToFuture yield
      assert(checkedProblem == Left(problem))

  "liftProblem HttpException without Problem" in:
    for tried <- liftProblem(Task.raiseError[Int](withoutProblem)).materialize.runToFuture yield
      assert(tried == Success(Left(Problem(withoutProblem.getMessage))))

  "liftProblem with HttpException without Problem but getMessage is null" in:
    // This is not expected. A HttpException should have a message.
    val exception = new HttpClient.HttpException:
      def statusInt = 503
      val problem = None
      override def getMessage = null/*default when no message is given*/

    for tried <- liftProblem(Task.raiseError[Int](exception)).materialize.runToFuture yield
      assert(tried == Failure(exception))

  "liftProblem and failureToChecked save HTTP status code of the withoutProblem Exception" in:
    val Success(Left(problem)) = HttpClient.failureToChecked(Failure(withoutProblem)): @unchecked
    assert(problem.httpStatusCode == 503)
    assert(problem.toString == "WITHOUT PROBLEM")

    for case Success(Left(problem)) <- liftProblem(Task.raiseError[Int](withoutProblem)).materialize.runToFuture yield
      assert(problem.httpStatusCode == 503)

  "liftProblem with unknown Exception" in:
    val other = new Exception("OTHER")
    for tried <- liftProblem(Task.raiseError[Int](other)).materialize.runToFuture yield
      assert(tried == Failure(other))
