package com.sos.jobscheduler.common.http

import cats.syntax.flatMap._
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.http.RecouplingStreamReader._
import com.sos.jobscheduler.common.http.configuration.RecouplingStreamReaderConf
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.Observable
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/** Logs in, couples and fetches objects from a (HTTP) stream, and recouples after error. */
abstract class RecouplingStreamReader[@specialized(Long/*EventId or file position*/) I, V, Api <: SessionApi](
  toIndex: V => I,
  maybeUserAndPassword: Option[UserAndPassword],
  conf: RecouplingStreamReaderConf)
{
  protected def couple(index: I): Task[Checked[Completed]] =
    Task.pure(Checked.completed)

  protected def getObservable(api: Api, after: I): Task[Checked[Observable[V]]]

  protected def onCouplingFailed(api: Api, problem: Problem): Task[Completed] =
    Task {
      if (inUse.get && !stopRequested && !coupledApiVar.isStopped) {
        scribe.warn(s"$api: coupling failed: $problem")
      }
      scribe.debug(s"$api: coupling failed: $problem", problem.throwableOption.map(_.nullIfNoStackTrace).orNull)
      Completed
    }

  protected def onCoupled(api: Api, after: I): Task[Completed] =
    Task.pure(Completed)

  protected def eof(index: I) = false

  protected def idleTimeout = conf.timeout

  // TODO Genügt nicht `terminate` ?
  protected def stopRequested: Boolean

  private val coupledApiVar = new CoupledApiVar[Api]
  private val recouplingPause = new RecouplingPause
  private val inUse = AtomicBoolean(false)
  private var sinceLastTry = now - 1.hour

  /** Observes endlessly, recoupling and repeating when needed. */
  final def observe(api: Api, after: I): Observable[V] = {
    scribe.debug(s"$api: observe(after=$after)")
    if (inUse.getAndSet(true)) throw new IllegalStateException("RecouplingStreamReader can not be used concurrently")
    Observable.fromTask(decouple) >>
      new ForApi(api, after)
        .observeAgainAndAgain
        .guarantee(Task {
          inUse := false
        })
  }

  final def terminate: Task[Completed] =
    decouple >> coupledApiVar.terminate

  final def decouple: Task[Completed] =
    coupledApiVar.tryTake
      .flatMap {
        case None => Task.pure(Completed)
        case Some(api) => api.logoutOrTimeout(idleTimeout)
      }

  final def invalidateCoupledApi: Task[Completed] =
    coupledApiVar.invalidate

  final def coupledApi: Task[Api] =
    coupledApiVar.read

  final def pauseBeforeNextTry(delay: FiniteDuration): Task[Unit] =
    Task.defer {
      Task.sleep((sinceLastTry + delay).timeLeftOrZero roundUpToNext PauseGranularity)
    } >>
      Task {
        sinceLastTry = now  // update asynchronously
      }

  private final class ForApi(api: Api, initialAfter: I) {
    @volatile private var lastIndex = initialAfter

    def observeAgainAndAgain: Observable[V] =
      Observable.tailRecM(initialAfter) { after =>
        if (eof(after) || stopRequested || coupledApiVar.isStopped || !inUse.get)
          Observable.pure(Right(Observable.empty))
        else
          // Memory leak due to https://github.com/monix/monix/issues/791 ???
          Observable.pure(Right(
            observe(after)
              .doOnError(t => onCouplingFailed(api, Problem.pure(t)).void)
              .onErrorHandleWith { _ => Observable.empty }
          )) ++
            (Observable.fromTask(pauseBeforeNextTry(conf.delay)) >>
              Observable.delay(Left(lastIndex)))
      }.flatten

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
        if ((stopRequested || coupledApiVar.isStopped) || !inUse.get)
          Task.pure(Right(Observable.raiseError(new IllegalStateException(s"RecouplingStreamReader($api) has been stopped"))))
        else
          coupleIfNeeded(after = after) >>
            getObservableX(after = after)
              .materialize.map(Checked.flattenTryChecked)
              .flatMap {
                case Left(problem) =>
                  (problem match {
                    case InvalidSessionTokenProblem =>
                      scribe.debug(s"$api: $InvalidSessionTokenProblem")
                      decouple
                    case _ =>
                      onCouplingFailed(api, problem) >> decouple
                  }) >>
                    pauseBeforeRecoupling >>
                    Task.pure(Left(()))
                case Right(observable) =>
                  Task.pure(Right(observable))
              })

    private def getObservableX(after: I): Task[Checked[Observable[V]]] =
      Task {
        sinceLastTry = now
      } >>
        getObservable(api, after = after)
          .timeout(idleTimeout)
          .onErrorRecoverWith { case t: TimeoutException =>
            scribe.debug(s"$api: ${t.toString}")
            Task.pure(Right(Observable.empty))
          }
          .map(_.map(
            _.timeoutOnSlowUpstream(idleTimeout + 1.s/*Let server time-out an idle stream first*/)
              .onErrorRecoverWith { case t: UpstreamTimeoutException =>
                scribe.debug(s"$api: ${t.toString}")
                Observable.empty
              }))

    private def coupleIfNeeded(after: I): Task[Completed] =
      coupledApiVar.tryRead.flatMap {
        case Some(_) => Task.pure(Completed)
        case None => tryEndlesslyToCouple(after)
      }

    private def tryEndlesslyToCouple(after: I): Task[Completed] =
      Task.tailRecM(())(_ =>
        if (stopRequested || coupledApiVar.isStopped || !inUse.get)
          Task.raiseError(new IllegalStateException(s"RecouplingStreamReader($api) has been stopped") with NoStackTrace)
        else
          (for {
            otherCoupledClient <- coupledApiVar.tryRead
            _ <- otherCoupledClient.fold(Task.unit)(_ => Task.raiseError(new IllegalStateException("Coupling while already coupled")))
            _ <- Task { recouplingPause.onCouple() }
            _ <- api.login(maybeUserAndPassword, onlyIfNotLoggedIn = true).timeout(idleTimeout)
            checkedCompleted <- couple(index = lastIndex)
          } yield checkedCompleted)
            .materialize.map(o => Checked.flattenTryChecked(o))  // materializeIntoChecked
            .flatMap {
              case Left(problem) =>
                for {
                  _ <- api.logoutOrTimeout(idleTimeout)
                  _ <- onCouplingFailed(api, problem)
                  _ <- pauseBeforeRecoupling
                } yield Left(())

              case Right(Completed) =>
                for {
                  _ <- coupledApiVar.tryPut(api)
                  _ <- Task { recouplingPause.onCouplingSucceeded() }
                  completed <- onCoupled(api, after)
                } yield Right(completed)
            })}

  private val pauseBeforeRecoupling =
    Task.defer(pauseBeforeNextTry(recouplingPause.nextPause()))
}

object RecouplingStreamReader
{
  val TerminatedProblem = Problem.pure("RecouplingStreamReader has been stopped")
  private val PauseGranularity = 500.ms

  def observe[@specialized(Long/*EventId or file position*/) I, V, Api <: SessionApi](
    toIndex: V => I,
    api: Api,
    maybeUserAndPassword: Option[UserAndPassword],
    conf: RecouplingStreamReaderConf,
    after: I,
    getObservable: I => Task[Checked[Observable[V]]],
    eof: I => Boolean = (_: I) => false,
    stopRequested: () => Boolean = () => false)
    (implicit s: Scheduler)
  : Observable[V]
  = {
    val eof_ = eof
    val getObservable_ = getObservable
    val stopRequested_ = stopRequested
    new RecouplingStreamReader[I, V, Api](toIndex, maybeUserAndPassword, conf) {
      def getObservable(api: Api, after: I) = getObservable_(after)
      override def eof(index: I) = eof_(index)
      def stopRequested = stopRequested_()
    }.observe(api, after)
  }

  private class RecouplingPause {
    // This class may be used asynchronously but not concurrently
    private val Minimum = 1.s
    @volatile private var pauses = initial
    @volatile private var lastCouplingTriedAt = now

    def onCouple() = lastCouplingTriedAt = now
    def onCouplingSucceeded() = pauses = initial
    def nextPause() = (lastCouplingTriedAt + synchronized { pauses.next() }).timeLeft max Minimum

    private def initial = Iterator(Minimum, 1.s, 1.s, 1.s, 2.s, 5.s) ++
      Iterator.continually(10.s)
  }
}
