package js7.base.monixutils

import cats.Monoid
import cats.effect.{ExitCase, Resource}
import js7.base.generic.Completed
import js7.base.monixutils.MonixDeadline.monotonicClock
import js7.base.monixutils.MonixDeadline.syntax._
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import monix.eval.Task
import monix.execution.Ack.{Continue, Stop}
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Ack, Cancelable, Scheduler, UncaughtExceptionReporter}
import monix.reactive.{Observable, OverflowStrategy}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IterableFactory, IterableOps}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise, TimeoutException}
import scala.util.chaining.scalaUtilChainingOps

object MonixBase
{
  private val FalseTask = Task.pure(false)
  private val TrueTask = Task.pure(true)
  private val CompletedTask = Task.pure(Completed)
  val DefaultBatchSize = 256
  val DefaultWorryDurations = Seq(3.s, 7.s, 10.s)
  private val logger = scribe.Logger(getClass.scalaName)

  object syntax
  {
    implicit class RichTaskCompanion(private val underlying: Task.type) extends AnyVal {
      def False = FalseTask
      def True = TrueTask
      def completed = CompletedTask
    }

    implicit class RichMonixTask[A](private val task: Task[A]) extends AnyVal
    {
      def when(condition: Boolean)(implicit A: Monoid[A]): Task[A] =
        if (condition)
          task
        else
          Task.pure(A.empty)

      def unless(condition: Boolean)(implicit A: Monoid[A]): Task[A] =
        when(!condition)

      def maybeTimeout(duration: Duration): Task[A] =
        duration match {
          case d: FiniteDuration => task.timeout(d)
          case _ => task
        }

      def orTimeout(timeout: Duration, onTimeout: => Task[A]): Task[A] =
        timeout match {
          case d: FiniteDuration =>
            task.timeout(d)
              .onErrorRecoverWith { case _: TimeoutException =>
                onTimeout
              }
          case _ => task
        }

      def logWhenItTakesLonger(what: String): Task[A] =
        task.whenItTakesLonger()(duration => Task {
          def msg = s"Still waiting for $what for ${duration.pretty} ..."
          if (duration < 10.s) scribe.debug(msg)
          else scribe.info(msg)
        })

      /** When `this` takes longer than `duration` then call `thenDo` once. */
      def whenItTakesLonger(duration: FiniteDuration)(thenDo: Task[Unit]): Task[A] =
        if (!duration.isPositive)
          task
        else
          whenItTakesLonger(duration :: ZeroDuration :: Nil)(_ => thenDo)

      /** As long as `this` has not completed, call `thenDo` after each of `durations` .
        * @param durations if empty then `thenDo` will not be called.
        *                  The last entry is repeated until `this` completes.
        *                  A zero or negative duration terminates calling of `thenDo`.
        * @param thenDo A function which gets the elapsed time since start as argument. */
      def whenItTakesLonger(durations: IterableOnce[FiniteDuration] = DefaultWorryDurations)
        (thenDo: FiniteDuration => Task[Unit])
      : Task[A] = {
        val durationIterator = durations.iterator
        if (durationIterator.isEmpty)
          task
        else
          monotonicClock.flatMap(since =>
            Task
              .tailRecM(ZeroDuration) { lastDuration =>
                val d = durationIterator.nextOption() getOrElse lastDuration
                if (d.isPositive)
                  Task.sleep(d)
                    .flatMap(_ => thenDo(since.elapsed))
                    .map(_ => Left(d))
                else
                  Task.pure(Right(()))
              }
              .start
              .bracket(_ => task)(_.cancel))
      }
    }

    implicit class RichMonixObservableCompanion(private val underlying: Observable.type) extends AnyVal
    {
      /** Provides the Scheduler, similar to Task deferAction. */
      def deferAction[A](toObservable: Scheduler => Observable[A]): Observable[A] =
        Observable
          .fromTask(
            Task.deferAction(implicit scheduler =>
              Task(toObservable(scheduler))))
          .flatten
    }

    implicit class RichMonixObservable[A](private val underlying: Observable[A]) extends AnyVal
    {
      def toL[Col[x] <: IterableOps[x, Iterable, Iterable[x]]](implicit factory: IterableFactory[Col]): Task[Col[A @uncheckedVariance]] =
        underlying.foldLeftL(factory.newBuilder[A])(_ += _).map(_.result())

      def tapEach(f: A => Unit): Observable[A] =
        underlying.map { a =>
          f(a)
          a
        }

      // Copied from Monix echoRepeated
      /** Mirror the source observable as long as the source keeps emitting
        * items, otherwise if `timeout` passes without the source emitting
        * anything new then the observable will start emitting
        * `intersperseValue` repeatedly.
        * Different from `echoRepeated`, this inserts hearbeats from start
        *
        * Note: If the source Observable keeps emitting items more
        * frequently than the length of the time window then the resulting
        * observable will mirror the source exactly.
        *
        * @param timeout the window of silence that must pass in order for the
        *        observable to start echoing the last item
        */
      final def insertHeartbeatsOnSlowUpstream(timeout: FiniteDuration, heartbeatValue: A): Observable[A] =
        new InsertHeartbeatsOnSlowUpstream[A](
          heartbeatValue +: underlying,   // Insert heartbeats from start
          timeout, onlyOnce = false, heartbeatValue
        ).drop(1)  // Remove inserted initial heartbeat

      def mapParallelOrderedBatch[B](
        batchSize: Int = DefaultBatchSize,
        delay: FiniteDuration = FiniteDuration.MaxValue,
        parallelism: Int = sys.runtime.availableProcessors)
        (f: A => B)
        (implicit os: OverflowStrategy[B] = OverflowStrategy.Default)
      : Observable[B] =
        if (!delay.isPositive || parallelism == 1)
          underlying.map(f)
        else
          underlying
            .pipe(obs =>
              if (delay < FiniteDuration.MaxValue)
                obs.buffer(Some(delay), batchSize)
              else
                obs.bufferTumbling(batchSize))
            .mapParallelOrdered(parallelism)(seq => Task(seq map f))
            .flatMap(Observable.fromIterable)

      //def mapParallelOrderedTimedBatch[B](
      //  maxDelay: FiniteDuration,
      //  batchSize: Int = DefaultBatchSize,
      //  parallelism: Int = sys.runtime.availableProcessors)
      //  (f: A => B)
      //  (implicit os: OverflowStrategy[B] = OverflowStrategy.Default)
      //: Observable[Seq[B]] =
      //  if (maxDelay <= ZeroDuration)
      //    underlying.map(f)
      //  else
      //    underlying
      //      .bufferTimedAndCounted(maxDelay, batchSize)
      //      .mapParallelOrdered(parallelism)(seq => Task(seq map f))

      def mapParallelUnorderedBatch[B](
        batchSize: Int = DefaultBatchSize,
        parallelism: Int = sys.runtime.availableProcessors)
        (f: A => B)
        (implicit os: OverflowStrategy[B] = OverflowStrategy.Default)
      : Observable[B] =
        if (parallelism == 1)
          underlying.map(f)
        else
          underlying
            .bufferTumbling(batchSize)
            .mapParallelUnordered(parallelism)(seq => Task(seq map f))
            .flatMap(Observable.fromIterable)

       def updateState[S](seed: S)(f: (S, A) => S): Observable[(S, A)] =
         underlying.scan((seed, null.asInstanceOf[A])) {
           case ((state, _), a) => f(state, a) -> a
         }

       def updateStateWhileInclusive[S](seed: S)(predicate: S => Boolean)(f: (S, A) => S): Observable[A] = {
         updateState(seed)(f)
         .takeWhileInclusive(o => predicate(o._1))
         .map(_._2)
       }

      def logTiming(
        toCount: A => Long = simpleCount,
        onComplete: (FiniteDuration, Long, ExitCase[Throwable]) => Unit,
        startedAt: Deadline = now)
      : Observable[A] =
        Observable
          .fromTask(
            Task.pure(underlying)
              .logTiming(toCount, onComplete, startedAt))
          .flatten

      final def buffer(timespan: Option[FiniteDuration], maxCount: Long, toWeight: A => Long = _ => 1): Observable[Seq[A]] =
        new BufferedObservable[A](underlying, timespan, maxCount, toWeight)
    }

    implicit class RichMonixAckFuture(private val ack: Future[Ack]) extends AnyVal {
      def syncFlatMapOnContinue(body: => Future[Ack])(implicit u: UncaughtExceptionReporter) =
        ack.syncTryFlatten.syncFlatMap {
          case Continue => body
          case Stop =>
            logger.debug("Ack.syncFlatMapOnContinue ignored because Observable returned Stop")
            Stop
        }
    }

    implicit class RichMonixObservableTask[A](private val underlying: Task[Observable[A]]) extends AnyVal
    {
      def logTiming(
        toCount: A => Long = simpleCount,
        onComplete: (FiniteDuration, Long, ExitCase[Throwable]) => Unit,
        startedAt: Deadline = now)
      : Task[Observable[A]] =
        underlying
          .map(Right(_))
          .logTiming(toCount, onComplete, startedAt)
          .map(_.orThrow/*never throws*/)
    }

    implicit class RichMonixObservableCheckedTask[A](private val underlying: Task[Checked[Observable[A]]])
    extends AnyVal
    {
      def logTiming(
        toCount: A => Long = simpleCount,
        onComplete: (FiniteDuration, Long, ExitCase[Throwable]) => Unit,
        startedAt: => Deadline = now)
      : Task[Checked[Observable[A]]] =
        Task.defer {
          val startedAt_ = startedAt
          var counter = 0L
          underlying.map(_.map(_
            .map { a => counter += toCount(a); a }
            .guaranteeCase(exitCase => Task(onComplete(startedAt_.elapsed, counter, exitCase)))))
        }
    }

    implicit class RichObserableIterable[A](private val underlying: Iterable[A]) extends AnyVal
    {
      def toObservable: Observable[A] =
        Observable.fromIterable(underlying)
    }

    private def simpleCount[A](a: A) = 1L

    implicit class RichCheckedTask[A](private val underlying: Task[Checked[A]]) extends AnyVal
    {
      /** Converts a failed Task into a `Task[Left[Throwable]]`. */
      def materializeIntoChecked: Task[Checked[A]] =
        underlying.materialize.map(Checked.flattenTryChecked)
    }

    implicit final class RichScheduler(private val underlying: Scheduler) extends AnyVal
    {
      def scheduleAtFixedRates(durations: IterableOnce[FiniteDuration])(body: => Unit): Cancelable = {
        val cancelable = SerialCancelable()
        val iterator = durations.iterator
        def loop(last: MonixDeadline): Unit = {
          val nextDuration = iterator.next()
          val next = last + nextDuration
          val delay = next - underlying.now
          cancelable := (
            if (iterator.hasNext)
              underlying.scheduleOnce(delay) {
                body
                loop(next)
              }
            else
              underlying.scheduleAtFixedRate(delay, nextDuration)(body))
        }
        if (iterator.hasNext) {
          loop(underlying.now)
        }
        cancelable
      }
    }
  }

  import syntax._

  def promiseTask[A](body: Promise[A] => Unit): Task[A] =
    Task.deferFuture {
      val promise = Promise[A]()
      body(promise)
      promise.future
    }

  def durationOfTask[A](task: Task[A]): Task[(A, FiniteDuration)] =
    Task.deferAction { implicit s =>
      val t = now
      task.map(_ -> t.elapsed)
    }

  //def deferFutureAndLog[A](f: => Future[A])(implicit A: TypeTag[A]): Task[A] =
  //  deferFutureAndLog(A.tpe.toString, f)

  def deferFutureAndLog[A](lazyFuture: => Future[A], name: => String): Task[A] =
    Task.defer {
      val future = lazyFuture
      future.value match {
        case Some(tried) =>
          Task.fromTry(tried)

        case None =>
          logger.debug(s"Waiting for Future '$name' ...")
          Task.deferAction { implicit s =>
            Task.fromFuture(future)
              .whenItTakesLonger()(duration => Task {
                logger.info(s"Still waiting for '$name' for ${duration.pretty} ...")
              })
              .guaranteeCase(exitCase => Task(logger.debug(s"Future '$name' $exitCase")))
          }
      }
    }

  def autoCloseableToObservable[A <: AutoCloseable](newA: => A): Observable[A] =
    Observable.fromResource(Resource.fromAutoCloseable(Task(newA)))

  def closeableIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] =
    closingIteratorToObservable(iterator.closeAtEnd)

  private def closingIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] =
    Observable.fromIterator(Task(iterator))
      .guaranteeCase { exitCase =>
        Task {
          logger.trace(s"Close $iterator $exitCase")
          iterator.close()
        }
      }

  /** Like Observable tailRecM, but limits the memory leak.
    * After a number of `Left` retured by `f`, the returned `Observable` is truncated.
    *
    * @see see Monix 3.2.1, https://github.com/monix/monix/issues/791
    */
  def memoryLeakLimitedObservableTailRecM[A, B](a: A, limit: Int)(f: A => Observable[Either[A, B]]): Observable[B] =
    Observable.defer {
      var leftCounter = 0
      Observable.tailRecM(a)(a =>
        f(a).flatMap {
          case o @ Left(_) =>
            if (leftCounter >= limit) {
              logger.debug(s"Limit Observable.tailRecM after $leftCounter× Left to reduce memory leakage")
              Observable.empty
            } else {
              leftCounter += 1
              Observable.pure(o)
            }

          case o => Observable.pure(o)
        })
    }
}
