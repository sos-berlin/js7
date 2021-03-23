package js7.tests.feed

import cats.effect.Resource
import java.io.InputStream
import js7.base.generic.Completed
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.problem.Checked
import js7.base.thread.Futures.implicits.SuccessFuture
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

object FeedMain
{
  def main(args: Array[String]): Unit = {
    coupleScribeWithSlf4j()

    if (args.isEmpty || args.sameElements(Array("--help"))) {
      println("Usage: testAddOrders --workflow=WORKFLOWPATH --order-count=1 --user=USER:PASSWORD")
    } else {
      run(args, Resource.eval(Task.pure(System.in)))
        .runToFuture
        .awaitInfinite
        match {
          case Left(problem) =>
            println(problem.toString)
            System.exit(1)

          case Right(Completed) =>
        }
    }
  }

  def run(args: Array[String], in: Resource[Task, InputStream]): Task[Checked[Completed]] = {
    val settings = Settings.parseArguments(args.toSeq)
    Feed.run(in, settings)
  }
}
