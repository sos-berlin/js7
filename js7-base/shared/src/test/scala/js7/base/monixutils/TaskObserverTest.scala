package js7.base.monixutils

import cats.effect.IO
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.subjects.PublishSubject
import js7.base.test.OurAsyncTestSuite

final class IOObserverTest extends OurAsyncTestSuite:
  "send" in:
    val subject = PublishSubject[Int]()
    val observing = subject.toListL.runToFuture
    val n = 10
    val ioObserver = IOObserver(subject)
    val send = IO.tailRecM(1)(i =>
      if i <= n then
        ioObserver.send(i).as(Left(i + 1))
      else
        IO.pure(Right(())) )
    for
      _ <- (send >> ioObserver.complete).runToFuture
      result <- observing
    yield assert(result == (1 to n))
