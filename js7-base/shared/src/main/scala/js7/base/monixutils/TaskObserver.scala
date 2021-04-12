package js7.base.monixutils

import monix.eval.Task
import monix.execution.Ack
import monix.execution.Ack.{Continue, Stop}
import monix.reactive.Observer
import scala.concurrent.Future
import scala.util.Success

final class TaskObserver[A] private(observer: Observer[A])
{ self =>
  @volatile private var ack: Future[Ack] = Continue
  @volatile private var completed = false

  def send(a: A): Task[Ack] =
    Task.deferFutureAction(implicit s =>
      self.synchronized {
        ack match {
          case Continue =>
            ack = observer.onNext(a)
            ack

          case Stop =>
            Stop

          case _ =>
            if (ack.isCompleted)
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

  def sync: Task[Unit] =
    Task
      .deferFutureAction(implicit s =>
        ack.syncMap(identity))
      .as(())

  def complete: Task[Unit] =
    Task.defer {
      self.synchronized {
        if (completed)
          Task.unit
        else {
          completed = true
          Task
            .deferFutureAction { implicit s =>
              ack = ack.syncTryFlatten.syncMap { _ =>
                observer.onComplete()
                Stop
              }
              ack
            }
            .as(())
        }
      }
   }
}

object TaskObserver
{
  def apply[A](observer: Observer[A]): TaskObserver[A] =
    new TaskObserver(observer)
}
