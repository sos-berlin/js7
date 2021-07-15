package js7.common.akkahttp

import akka.util.ByteString
import js7.common.akkahttp.ByteStringSplittingObservable._
import monix.execution.Ack.{Continue, Stop}
import monix.execution.cancelables.CompositeCancelable
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import scala.annotation.tailrec
import scala.concurrent.Future

private final class ByteStringSplittingObservable(source: Observable[ByteString], maxSize: Int)
extends Observable[ByteString]
{
  //<editor-fold desc="NOT USED">

  def unsafeSubscribeFn(out: Subscriber[ByteString]) = {
    val subscriber = new SplittingSubscriber(maxSize, out)
    val subscription = source.unsafeSubscribeFn(subscriber)
    CompositeCancelable(subscriber, subscription)
  }
  //</editor-fold>
}

object ByteStringSplittingObservable
{
  private final class SplittingSubscriber(maxSize: Int, out: Subscriber[ByteString])
  extends Subscriber[ByteString] with Cancelable
  {
    //<editor-fold desc="NOT USED">

    @volatile private var canceled = false
    implicit def scheduler = out.scheduler
    private var ack: Future[Ack] = Continue
    private var isDone = false

    def cancel() =
      canceled = true

    def onNext(elem: ByteString) =
      forward(elem)

    private def forward(elem: ByteString): Future[Ack] =
      synchronized {
        ack = tailRecursiveForward(elem)
        ack
      }

    @tailrec
    private def tailRecursiveForward(elem: ByteString): Future[Ack] =
      if (elem.isEmpty)
        Continue
      else if (canceled)
        Stop
      else {
        val (head, tail) = elem.splitAt(maxSize)
        out.onNext(head).syncTryFlatten match {
          case Stop => Stop
          case Continue => tailRecursiveForward(tail)
          case ack: Future[Ack] =>
            ack.flatMap {
              case Stop => Stop
              case Continue => forward(tail)
            }
        }
      }

    def onError(t: Throwable) =
      if (!isDone) synchronized {
        isDone = true
        ack = Stop
        out.onError(t)
      }

    def onComplete() =
      if (!isDone) synchronized {
        isDone = true
        ack = for (_ <- ack) yield {
          out.onComplete()
          Stop
        }
      }
    //</editor-fold>
  }
}
