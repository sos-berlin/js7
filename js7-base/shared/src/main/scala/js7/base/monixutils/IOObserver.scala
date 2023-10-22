package js7.base.monixutils

import cats.effect.IO
import monix.execution.Ack
import monix.execution.Ack.{Continue, Stop}
import monix.reactive.Observer
import scala.concurrent.Future
import scala.util.Success

final class IOObserver[A] private(observer: Observer[A])
{ self =>
  @volatile private var ack: Future[Ack] = Continue
  @volatile private var completed = false

  def send(a: A): IO[Ack] =
    IO.deferFutureAction(implicit s =>
      self.synchronized {
        ack match {
          case Continue =>
            ack = observer.onNext(a)
            ack

          case Stop =>
            Stop

          case _ =>
            if ack.isCompleted then
              ack.value.get match {
                case Success(Continue) =>
                  ack = observer.onNext(a)
                  ack
                case o =>
                  Future.fromTry(o)
              }
            else
              ack.flatMap {
                case Continue =>
                  ack = observer.onNext(a)
                  ack
                case o =>
                  o
              }
        }
      })

  def sync: IO[Unit] =
    IO
      .deferFutureAction(implicit s =>
        ack.syncMap(identity))
      .void

  def complete: IO[Unit] =
    IO.defer:
      self.synchronized:
        if completed then
          IO.unit
        else
          completed = true
          IO
            .deferFutureAction { implicit s =>
              ack = ack.syncTryFlatten.syncMap { _ =>
                observer.onComplete()
                Stop
              }
              ack
            }
            .void
}


object IOObserver:
  def apply[A](observer: Observer[A]): IOObserver[A] =
    new IOObserver(observer)
