package js7.base.monixutils

import cats.Monoid
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import js7.base.generic.Completed
import js7.base.monixutils.MonixDeadline.monotonicClock
import js7.base.monixutils.MonixDeadline.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.Task
import monix.execution.Ack.{Continue, Stop}
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Ack, Cancelable, Scheduler, UncaughtExceptionReporter}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.BackPressure
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IterableFactory, IterableOps}
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{Future, Promise}

object MonixBase
{
  private val FalseTask = Task.pure(false)
  private val TrueTask = Task.pure(true)
  private val CompletedTask = Task.pure(Completed)
  val DefaultBatchSize = 256
  val DefaultWorryDurations = Seq(3.s, 7.s, 10.s)
  /** Log worry at info level after this duration. */
  val InfoWorryDuration = DefaultWorryDurations.last
  private val logger = js7.base.log.Logger[this.type]

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

      def cancelWhen[B >: A](canceler: Task[B]): Task[B] =
        Task
          .racePair(canceler.uncancelable, task)
          .flatMap {
            case Left((canceled, fiber)) => fiber.cancel.as(canceled)
            case Right((_, canceled)) => Task.pure(canceled)
          }

      def startAndForgetOrLog(what: => String): Task[Unit] =
        task
          .logAndIgnoreError(what)
          .startAndForget

      def runAsyncAndForgetOrLog(what: => String)(implicit scheduler: Scheduler): Unit =
        task
          .logAndIgnoreError(what)
          .runAsyncAndForget

      def logAndIgnoreError(what: => String): Task[Unit] =
        task.void
          .onErrorHandle(t =>
            logger.error(s"$what => ${t.toStringWithCauses}", t.nullIfNoStackTrace))

      def logWhenItTakesLonger(implicit enclosing: sourcecode.Enclosing): Task[A] =
        logWhenItTakesLonger2("in", "continues", enclosing.value)

      def logWhenItTakesLonger(what: => String): Task[A] =
        logWhenItTakesLonger2("for", "completed", what)

      private def logWhenItTakesLonger2(preposition: String, completed: String, what: => String)
      : Task[A] =
        Task.defer {
          val since = now
          var infoLogged = false
          task
            .whenItTakesLonger()(duration => Task {
              val m = if (duration < InfoWorryDuration) "🟡" else "🟠"
              def msg = s"$m Still waiting $preposition $what for ${duration.pretty}"
              if (duration < InfoWorryDuration)
                logger.debug(msg)
              else {
                infoLogged = true
                logger.info(msg)
              }
            })
            .guaranteeCase(exit => Task(if (infoLogged) exit match {
              case ExitCase.Completed => logger.info(
                s"🔵 $what $completed after ${since.elapsed.pretty}")
              case ExitCase.Canceled => logger.info(
                s"⚫ $what canceled after ${since.elapsed.pretty}")
              case ExitCase.Error(t) => logger.info(
                s"💥 $what failed after ${since.elapsed.pretty} with ${t.toStringWithCauses}")
            }))
        }

      /** When `this` takes longer than `duration` then call `thenDo` once. */
      def whenItTakesLonger(duration: FiniteDuration)(thenDo: Task[Unit]): Task[A] =
        if (duration.isZeroOrBelow)
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
                    .*>(Task(since.elapsed))
                    .flatMap(thenDo)
                    .as(Left(d))
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
      /** Like toListL, but for an IterableFactory. */
      def toL[Col[x] <: IterableOps[x, Iterable, Iterable[x]]](factory: IterableFactory[Col])
      : Task[Col[A @uncheckedVariance]] =
        underlying
          .foldLeftL(factory.newBuilder[A])(_ += _)
          .map(_.result())

      def tapEval(f: A => Task[Unit]): Observable[A] =
        underlying.flatTap(a => Observable.fromTask(f(a)))

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

      def mapParallelBatch[B](
        batchSize: Int = DefaultBatchSize,
        responsive: Boolean = false,
        parallelism: Int = sys.runtime.availableProcessors)
        (f: A => B)
      : Observable[B] = {
        val minimumBackPressure = BackPressure(parallelism max 2)
        if (batchSize <= 0 || parallelism <= 1)
          underlying.map(f)
        else if (batchSize == 1)
          underlying.mapParallelOrdered(parallelism)(a => Task(f(a)))(
            minimumBackPressure)
        else if (responsive)
          underlying
            .mapParallelOrdered(parallelism)(a => Task(f(a)))(
              BackPressure(parallelism * batchSize max 2))
        else
          underlying
            .bufferTumbling(batchSize)
            .mapParallelOrdered(parallelism)(seq => Task(seq.map(f)))(
              minimumBackPressure)
            .flatMap(Observable.fromIterable)
      }

      def mapParallelUnorderedBatch[B](
        batchSize: Int = DefaultBatchSize,
        parallelism: Int = sys.runtime.availableProcessors)
        (f: A => B)
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

    implicit class RichCheckedTask[A](private val task: Task[Checked[A]]) extends AnyVal
    {
      /** Converts a failed Task into a `Task[Left[Throwable]]`. */
      def materializeIntoChecked: Task[Checked[A]] =
        task.materialize.map(Checked.flattenTryChecked)

      def orThrow: Task[A] =
        task.flatMap {
          case Left(problem) => Task.raiseError(problem.throwable)
          case Right(a) => Task.pure(a)
        }

      def guaranteeExceptWhenRight(release: Task[Unit]): Task[Checked[A]] =
        task
          .guaranteeCase {
            case ExitCase.Completed => Task.unit
            case _ => release
          }
          .flatTap {
            case Left(problem) => release.as(Left(problem))
            case Right(_: A) => Task.unit
          }
    }

    implicit final class RichMonixResource[A](private val resource: Resource[Task, A])
    extends AnyVal
    {
      def executeOn(scheduler: Scheduler): Resource[Task, A] =
        Resource.suspend(
          // Execute acquire and release on the given `scheduler`
          resource
            .allocated
            .map { case (acquiredThing, release) =>
              Resource.make(
                acquire = Task.pure(acquiredThing))(
                release = _ => release.executeOn(scheduler))
            }
            .executeOn(scheduler))
    }

    implicit final class RichScheduler(private val scheduler: Scheduler) extends AnyVal
    {
      def timestamp: Timestamp =
        Timestamp.ofEpochMilli(scheduler.clockRealTime(MILLISECONDS))

      def scheduleAtFixedRates(durations: IterableOnce[FiniteDuration])(body: => Unit): Cancelable = {
        val cancelable = SerialCancelable()
        val iterator = durations.iterator
        def loop(last: MonixDeadline): Unit = {
          val nextDuration = iterator.next()
          val next = last + nextDuration
          val delay = next - scheduler.now
          cancelable := (
            if (iterator.hasNext)
              scheduler.scheduleOnce(delay) {
                body
                loop(next)
              }
            else
              scheduler.scheduleAtFixedRate(delay, nextDuration)(body))
        }
        if (iterator.hasNext) {
          loop(scheduler.now)
        }
        cancelable
      }
    }
  }

  def promiseTask[A](body: Promise[A] => Unit): Task[A] =
    Task.deferFuture {
      val promise = Promise[A]()
      body(promise)
      promise.future
    }

  def durationOfTask[A](task: Task[A]): Task[(A, FiniteDuration)] =
    Task.defer {
      val t = now
      task.map(_ -> t.elapsed)
    }

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
