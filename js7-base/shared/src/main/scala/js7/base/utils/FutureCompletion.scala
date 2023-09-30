package js7.base.utils

import cats.effect.Resource
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

/**
  * A register for `Future` completion which allows removal of handlers.
  *
  * This avoids the memory leak in `Future.onComplete` which does not allow removal.
  *
  * @author Joacim Zschimmer
  */
final class FutureCompletion[A](future: Future[A])(implicit ec: ExecutionContext):

  private val numberToEntry = mutable.Map.empty[Int, Entry]
  private val counter = AtomicInt(0)
  @volatile private var completed: Option[Try[A]] = None

  future.onComplete { tried =>
    numberToEntry.synchronized:
      completed = Some(tried)
      for entry <- numberToEntry.values do
        entry.promise.complete(tried)
  }

  def observable: Observable[Observable[A]] =
    Observable.fromResource(resource)
      .map(o => Observable.fromFuture(o))

  def resource: Resource[Task, Future[A]] =
    Resource.fromAutoCloseable(Task(add()))
      .map(_.future)

  private[utils] def add(): Entry =
    val entry = new Entry
    val wasCompleted = numberToEntry.synchronized:
      if completed.isEmpty then
        numberToEntry.update(entry.number, entry)
      completed
    wasCompleted match
      case None =>
        entry.promise.future onComplete { _ =>
          remove(entry)
        }
      case Some(tried) =>
        entry.promise.complete(tried)
    entry

  private def remove(entry: Entry): Unit =
    numberToEntry.synchronized:
      numberToEntry.remove(entry.number)

  @TestOnly
  private[utils] def size: Int =
    numberToEntry.synchronized:
      numberToEntry.size

  final class Entry private[FutureCompletion] extends AutoCloseable:
    private[FutureCompletion] val promise = Promise[A]()
    private[FutureCompletion] val number = counter.incrementAndGet()

    def close() = remove(this)

    def future = promise.future

object FutureCompletion:
  object syntax:
    implicit final class FutureCompletionObservable[A](private val observable: Observable[A]) extends AnyVal:
      def takeUntilCompleted[B](futureCompletion: FutureCompletion[B]) =
        futureCompletion.observable flatMap observable.takeUntil

      def takeUntilCompletedAndDo[B](futureCompletion: FutureCompletion[B])(onCompleted: B => Task[Unit]) =
        futureCompletion.observable
          .flatMap(trigger => observable.takeUntil(trigger.doOnNext(onCompleted)))

    implicit final class FutureCompletionFuture[A](private val future: CancelableFuture[A]) extends AnyVal:
      def cancelOnCompletionOf[B](futureCompletion: FutureCompletion[B])(implicit s: Scheduler)
      : CancelableFuture[A] =
        val entry = futureCompletion.add()
        entry.future.onComplete { _ =>
          future.cancel()
        }
        future.transform { tried =>
          entry.close()
          tried
        }
