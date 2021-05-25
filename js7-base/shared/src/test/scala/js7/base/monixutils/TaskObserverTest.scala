package js7.base.monixutils

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import org.scalatest.freespec.AsyncFreeSpec

final class TaskObserverTest extends AsyncFreeSpec
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
