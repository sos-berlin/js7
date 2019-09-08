package com.sos.jobscheduler.core.event

import cats.instances.all._
import cats.syntax.all._
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Problems.InvalidSessionTokenProblem
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.core.configuration.EventFetcherConf
import com.sos.jobscheduler.core.event.EventFetcher._
import com.sos.jobscheduler.data.event.EventId
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicBoolean
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.Observable
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

abstract class EventFetcher[E, A, Api <: SessionApi](
  toEventId: A => EventId,
  maybeUserAndPassword: Option[UserAndPassword],
  conf: EventFetcherConf)
  (implicit s: Scheduler)
{
  protected def couple(eventId: EventId): Task[Checked[Completed]]

  protected def eventObservable(
    api: Api,
    after: EventId,
    timeout: Option[FiniteDuration] = Some(conf.eventFetchTimeout))
  : Task[Checked[Observable[A]]]

  protected def onCouplingFailed(problem: Problem): Task[Completed]

  protected def onCoupled: Task[Completed]

  protected def logEvent(a: A): Unit

  private var sinceLastFetch = now - 1.hour
  private val coupledApiMVar = MVar.empty[Task, Api]().runSyncUnsafe()
  private val recouplingPause = new RecouplingPause
  private val inUse = AtomicBoolean(false)

  /** Observes endlessly, recoupling and repeating when needed. */
  final def observe(api: Api, after: EventId): Observable[A] = {
    logger.debug(s"observe($api)")
    if (inUse.getAndSet(true)) throw new IllegalStateException("EventFetcher can not be used concurrently")
    Observable.fromTask(decouple) >>
      new ForApi(api).observeAgainAndAgain(after = after)
        .guarantee {
          Task { inUse := false }
        }
  }

  final def decouple: Task[Completed] =
    coupledApiMVar.tryTake.flatMap {
      case None => Task.pure(Completed)
      case Some(api) => api.logout().onErrorHandle(_ => Completed)
    }

  final def invalidateCoupledApi(): Unit =
    coupledApiMVar.tryTake.runSyncUnsafe()

  final def coupledApi: Task[Api] =
    coupledApiMVar.read

  final def delaySinceLastFetch(delay: FiniteDuration): FiniteDuration =
    delay - sinceLastFetch.elapsed

  private final class ForApi(api: Api) {
    @volatile private var lastEventId = EventId.BeforeFirst

    def observeAgainAndAgain(after: EventId) : Observable[A] = {
      lastEventId = after
      Observable.tailRecM(lastEventId)(after =>
        Observable.pure(Right(observe(after))) ++ Observable.delay(Left(lastEventId))
      ).flatten
    }

    private def observe(after: EventId): Observable[A] =
      Observable.fromTask(tryEndlesslyToGetObservable(after))
        .flatten
        .map { a =>
          lastEventId = toEventId(a)
          a
        }

    /** Retries until web request returns an Observable. */
    private def tryEndlesslyToGetObservable(after: EventId): Task[Observable[A]] =
      Task.tailRecM(())(_ =>
        coupleIfNeeded >>
          getObservable(after = after)
            .materializeIntoChecked
            .flatMap {
              case Left(problem) =>
                (problem match {
                  case InvalidSessionTokenProblem =>
                    logger.debug(InvalidSessionTokenProblem.toString)
                    decouple
                  case _ =>
                    logger.warn(problem.toString)
                    Task.unit
                }) >>
                  pauseBeforeRecoupling >>
                  Task.pure(Left(()))
              case Right(observable) =>
                Task.pure(Right(observable))
            })

    private def getObservable(after: EventId): Task[Checked[Observable[A]]] =
      Task {
        sinceLastFetch = now
      } >>
        eventObservable(api, after = after)
          .map(_.map(_
            .timeoutOnSlowUpstream(conf.eventFetchTimeout + TimeoutReserve)   // throws UpstreamTimeoutException
            .onErrorRecoverWith {
              case t: UpstreamTimeoutException =>
                logger.debug(t.toString)
                Observable.empty   // Let it look like end of stream
            }))

    private def coupleIfNeeded: Task[Completed] =
      coupledApiMVar.tryRead.flatMap {
        case Some(_) => Task.pure(Completed)
        case None => tryEndlesslyToCouple
      }

    private def tryEndlesslyToCouple: Task[Completed] =
      Task.tailRecM(())(_ =>
        (for {
          otherCoupledClient <- coupledApiMVar.tryRead
          _ <- otherCoupledClient.fold(Task.unit)(_ => Task.raiseError(new IllegalStateException("Coupling while already coupled")))
          _ <- Task { recouplingPause.onCouple() }
          _ <- api.login(maybeUserAndPassword)
          checkedCompleted <- couple(eventId = lastEventId)
        } yield checkedCompleted)
          .materialize.map(o => Checked.fromTry(o).flatten)
          .flatMap {
            case Left(problem) =>
              for {
                _ <- api.logout().onErrorHandle(_ => ())
                _ <- onCouplingFailed(problem)
                _ <- pauseBeforeRecoupling
              } yield Left(())

            case Right(Completed) =>
              for {
                _ <- coupledApiMVar.tryPut(api)
                _ <- Task {
                  recouplingPause.onCouplingSucceeded()
                  logger.info(s"Coupled with $api")
                }
                completed <- onCoupled
              } yield Right(completed)
          })}

  private val pauseBeforeRecoupling =
    Task { delaySinceLastFetch(recouplingPause.nextPause()) }
      .flatMap(Task.sleep)
}

object EventFetcher
{
  private val TimeoutReserve = 10.s
  private val logger = Logger(getClass)

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
