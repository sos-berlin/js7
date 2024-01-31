package js7.base.utils

import cats.effect.{IO, Resource}
import fs2.Stream
import java.util.concurrent.atomic.AtomicInteger
import org.jetbrains.annotations.TestOnly
import scala.annotation.nowarn
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
@deprecated("Avoid Future", "v2.7")
final class FutureCompletion[A](future: Future[A])(implicit ec: ExecutionContext):

  private val numberToEntry = mutable.Map.empty[Int, Entry]
  private val counter = Atomic(0)
  @volatile private var completed: Option[Try[A]] = None

  future.onComplete { tried =>
    numberToEntry.synchronized:
      completed = Some(tried)
      for entry <- numberToEntry.values do
        entry.promise.complete(tried)
  }

  def stream: Stream[IO, Stream[IO, A]] =
    Stream.resource(resource)
      .map(o => Stream.eval(IO.fromFuture(IO(o))))

  def resource: Resource[IO, Future[A]] =
    Resource.fromAutoCloseable(IO(add()))
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


@nowarn("msg=class FutureCompletion in package js7.base.utils is deprecated")
object FutureCompletion:
  object syntax:
    implicit final class FutureCompletionStream[A](private val stream: Stream[IO, A]) extends AnyVal:
      def takeUntilCompleted[B](futureCompletion: FutureCompletion[B]) =
        futureCompletion.stream.flatMap(trigger => stream.interruptWhen(trigger.as(true)))

      def takeUntilCompletedAndDo[B](futureCompletion: FutureCompletion[B])(onCompleted: B => IO[Unit]) =
        futureCompletion.stream
          .flatMap(trigger => stream.interruptWhen(trigger.evalTap(onCompleted).as(true)))

    implicit final class FutureCompletionFuture[A](private val future: CancelableFuture[A]) extends AnyVal:
      def cancelOnCompletionOf[B](futureCompletion: FutureCompletion[B])(using ExecutionContext)
      : CancelableFuture[A] =
        val entry = futureCompletion.add()
        entry.future.onComplete { _ =>
          future.cancelAndForget()
        }
        future.transform { tried =>
          entry.close()
          tried
        }