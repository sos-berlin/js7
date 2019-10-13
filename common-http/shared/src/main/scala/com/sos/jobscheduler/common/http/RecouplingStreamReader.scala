package com.sos.jobscheduler.common.http

import cats.syntax.flatMap._
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.http.RecouplingStreamReader._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/** Logs in, couples and fetches objects from a (HTTP) stream, and recouples after error. */
abstract class RecouplingStreamReader[@specialized(Long/*EventId or file position*/) I, V, Api <: SessionApi](
  zeroIndex: I,
  toIndex: V => I,
  maybeUserAndPassword: Option[UserAndPassword],
  conf: RecouplingStreamReaderConf)
  (implicit s: Scheduler)
{
  protected def couple(index: I): Task[Checked[Completed]] =
    Task.pure(Checked.completed)

  protected def getObservable(api: Api, after: I): Task[Checked[Observable[V]]]

  protected def onCouplingFailed(problem: Problem): Task[Completed] =
    Task.pure(Completed)

  protected def onCoupled(api: Api): Task[Completed] =
    Task.pure(Completed)

  protected def eof(index: I) = false

  protected def logEvent(a: V): Unit = {}

  private val coupledApiMVar = MVar.empty[Task, Api]().memoize
  private val recouplingPause = new RecouplingPause
  private val inUse = AtomicBoolean(false)
  private var sinceLastFetch = now - 1.hour

  /** Observes endlessly, recoupling and repeating when needed. */
  final def observe(api: Api, after: I): Observable[V] = {
    scribe.debug(s"observe($api)")
    if (inUse.getAndSet(true)) throw new IllegalStateException("RecouplingStreamReader can not be used concurrently")
    Observable.fromTask(decouple) >>
      new ForApi(api).observeAgainAndAgain(after = after)
        .guarantee {
          Task { inUse := false }
        }
  }

  final def decouple: Task[Completed] =
    coupledApiMVar.flatMap(_.tryTake)
      .flatMap {
        case None => Task.pure(Completed)
        case Some(api) => api.logout().onErrorHandle(_ => Completed)
      }

  final def invalidateCoupledApi: Task[Completed] =
    coupledApiMVar.flatMap(_.tryTake)
      .map(_ => Completed)

  final def coupledApi: Task[Api] =
    coupledApiMVar.flatMap(_.read)

  final def delaySinceLastFetch(delay: FiniteDuration): FiniteDuration =
    delay - sinceLastFetch.elapsed

  private final class ForApi(api: Api) {
    @volatile private var lastIndex = zeroIndex

    def observeAgainAndAgain(after: I) : Observable[V] = {
      lastIndex = after
      Observable.tailRecM(lastIndex)(after =>
        if (eof(after))
          Observable.pure(Right(Observable.empty))
        else
          Observable.pure(Right(observe(after))) ++
            Observable.evalDelayed(delaySinceLastFetch(conf.delay), Left(lastIndex))
      ).flatten
    }

    private def observe(after: I): Observable[V] =
      Observable.fromTask(tryEndlesslyToGetObservable(after))
        .flatten
        .map { a =>
          lastIndex = toIndex(a)
          a
        }

    /** Retries until web request returns an Observable. */
    private def tryEndlesslyToGetObservable(after: I): Task[Observable[V]] =
      Task.tailRecM(())(_ =>
        coupleIfNeeded >>
          getObservableX(after = after)
            .materialize.map(Checked.flattenTryChecked)
            .flatMap {
              case Left(problem) =>
                (problem match {
                  case InvalidSessionTokenProblem =>
                    scribe.debug(InvalidSessionTokenProblem.toString)
                    decouple
                  case _ =>
                    scribe.warn(problem.toString)
                    Task.unit
                }) >>
                  pauseBeforeRecoupling >>
                  Task.pure(Left(()))
              case Right(observable) =>
                Task.pure(Right(observable))
            })

    private def getObservableX(after: I): Task[Checked[Observable[V]]] =
      Task {
        sinceLastFetch = now
      } >>
        getObservable(api, after = after)
          .map(_.map(_
            .timeoutOnSlowUpstream(conf.timeout + TimeoutReserve)   // throws UpstreamTimeoutException
            .onErrorRecoverWith {
              case t: UpstreamTimeoutException =>
                scribe.debug(t.toString)
                Observable.empty   // Let it look like end of stream
            }))

    private def coupleIfNeeded: Task[Completed] =
      coupledApiMVar.flatMap(_.tryRead).flatMap {
        case Some(_) => Task.pure(Completed)
        case None => tryEndlesslyToCouple
      }

    private def tryEndlesslyToCouple: Task[Completed] =
      Task.tailRecM(())(_ =>
        (for {
          otherCoupledClient <- coupledApiMVar.flatMap(_.tryRead)
          _ <- otherCoupledClient.fold(Task.unit)(_ => Task.raiseError(new IllegalStateException("Coupling while already coupled")))
          _ <- Task { recouplingPause.onCouple() }
          _ <- if (api.hasSession) Task.unit else api.login(maybeUserAndPassword)
          checkedCompleted <- couple(index = lastIndex)
        } yield checkedCompleted)
          .materialize.map(o => Checked.flattenTryChecked(o))  // materializeIntoChecked
          .flatMap {
            case Left(problem) =>
              for {
                _ <- api.logout().onErrorHandle(_ => ())
                _ <- onCouplingFailed(problem)
                _ <- pauseBeforeRecoupling
              } yield Left(())

            case Right(Completed) =>
              for {
                _ <- coupledApiMVar.flatMap(_.tryPut(api))
                _ <- Task {
                  recouplingPause.onCouplingSucceeded()
                }
                completed <- onCoupled(api)
              } yield Right(completed)
          })}

  private val pauseBeforeRecoupling =
    Task { delaySinceLastFetch(recouplingPause.nextPause()) }
      .flatMap(Task.sleep)
}

object RecouplingStreamReader
{
  private val TimeoutReserve = 10.s

  def observe[@specialized(Long/*EventId or file position*/) I, V, Api <: SessionApi](
    zeroIndex: I,
    toIndex: V => I,
    api: Api,
    maybeUserAndPassword: Option[UserAndPassword],
    conf: RecouplingStreamReaderConf,
    after: I,
    getObservable: I => Task[Checked[Observable[V]]],
    eof: I => Boolean = (_: I) => false)
    (implicit s: Scheduler)
  : Observable[V]
  = {
    val eof_ = eof
    val getObservable_ = getObservable
    new RecouplingStreamReader[I, V, Api](zeroIndex, toIndex, maybeUserAndPassword, conf) {
      def getObservable(api: Api, after: I) = getObservable_(after)
      override def eof(index: I) = eof_(index)
    }.observe(api, after)
  }

  private class RecouplingPause {
    // This class may be used asynchronously but not concurrently
    private val Minimum = 1.second
    @volatile private var pauses = initial
    @volatile private var lastCouplingTriedAt = now

    def onCouple() = lastCouplingTriedAt = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCouplingTriedAt + synchronized { pauses.next() }).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.second, 1.second, 1.second, 2.seconds, 5.seconds) ++
      Iterator.continually(10.seconds)
  }
}
