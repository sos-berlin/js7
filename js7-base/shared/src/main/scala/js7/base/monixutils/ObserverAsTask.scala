package js7.base.monixutils

import js7.base.monixutils.MonixBase.syntax._
import monix.eval.Task
import monix.execution.Ack
import monix.execution.Ack.{Continue, Stop}
import monix.reactive.Observer
import scala.concurrent.Future

final class ObserverAsTask[A] private(observer: Observer[A])
{ self =>
  private var ack: Future[Ack] = Continue

  def send(a: A): Task[Ack] =
    Task.deferAction { implicit s =>
      self.synchronized {
        val result = Task.fromFuture(ack)
        ack = ack.syncFlatMapOnContinue(observer.onNext(a))
        result
      }
   }

  def sendOrRaise(a: A): Task[Unit] =
    Task.deferAction { implicit s =>
      send(a).flatMap {
        case Continue => Task.unit
        case Stop => Task.raiseError(new RuntimeException("Observer stopped"))
      }
    }
}

object ObserverAsTask
{
  def apply[A](observer: Observer[A]): ObserverAsTask[A] =
    new ObserverAsTask(observer)
}
