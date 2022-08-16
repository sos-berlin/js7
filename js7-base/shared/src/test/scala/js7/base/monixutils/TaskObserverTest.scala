package js7.base.monixutils

import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.subjects.PublishSubject
import js7.base.test.OurAsyncTestSuite

final class TaskObserverTest extends OurAsyncTestSuite
{
  "send" in {
    val subject = PublishSubject[Int]()
    val observing = subject.toListL.runToFuture
    val n = 10
    val taskObserver = TaskObserver(subject)
    val send = Task.tailRecM(1)(i =>
      if (i <= n)
        taskObserver.send(i).as(Left(i + 1))
      else
        Task.pure(Right(())) )
    for {
      _ <- (send >> taskObserver.complete).runToFuture
      result <- observing
    } yield assert(result == (1 to n))
  }
}
