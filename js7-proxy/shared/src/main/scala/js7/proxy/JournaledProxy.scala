package js7.proxy

import cats.effect.Resource
import cats.syntax.option.*
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.durationOfTask
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Problem, ProblemException}
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SetOnce
import js7.base.web.HttpClient
import js7.cluster.watch.api.{ActiveClusterNodeSelector, HttpClusterNodeApi}
import js7.common.http.RecouplingStreamReader
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventApi, EventId, EventRequest, EventSeqTornProblem, JournaledState, SnapshotableState, Stamped}
import js7.proxy.JournaledProxy.*
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.ProxyEvent.{ProxyCoupled, ProxyCouplingError, ProxyDecoupled}
import js7.proxy.data.event.{EventAndState, ProxyEvent, ProxyStarted}
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success}

trait JournaledProxy[S <: SnapshotableState[S]]
{
  protected val baseObservable: Observable[EventAndState[Event, S]]
  protected def scheduler: Scheduler
  protected val onEvent: EventAndState[Event, S] => Unit
  protected def S: SnapshotableState.Companion[S]
  protected def proxyConf: ProxyConf

  private val observing = SetOnce[Cancelable]("connectableObservingCompleted")
  private val stopRequested = Promise[Unit]()
  private val observingStopped = Promise[Unit]()

  private val currentStateFilled = Promise[Unit]()
  @volatile private var _currentState = S.empty

  private val connectableObservable: ConnectableObservable[EventAndState[Event, S]] =
    baseObservable
      .map { eventAndState =>
        _currentState = eventAndState.state
        currentStateFilled.trySuccess(())
        onEvent(eventAndState)
        eventAndState
      }
      .takeUntil(Observable.fromFuture(stopRequested.future))
      .doOnSubscriptionCancel(Task(
        logger.debug("connectableObservable: cancelling")))
      .publish(scheduler)

  final def observable: Observable[EventAndState[Event, S]] =
    connectableObservable

  final def startObserving: Task[Unit] = {
    val cancelable = SerialCancelable()
    Task.deferFutureAction { implicit scheduler =>
      assertThat(observing.isEmpty)
      val obs = connectableObservable.connect()
      observing := obs
      cancelable := obs
      val whenCompleted = connectableObservable.completedL.runToFuture
      observingStopped.completeWith(whenCompleted)
      whenCompleted.onComplete {
        case Success(()) =>
          if (!stopRequested.isCompleted) {
            logger.error("Observable has terminated")
          }
          // ???
        case Failure(t) =>
          logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          // ???
      }
      CancelableFuture(
        currentStateFilled.future,
        () => {
          logger.debug("startObserving: cancelling")
          whenCompleted.cancel()
          cancelable.cancel()
        })
    }
  }

  def sync(eventId: EventId): Task[Unit] =
    Task.defer {
      if (currentState.eventId >= eventId)
        Task.unit
      else
        Observable(
          observable.dropWhile(_.stampedEvent.eventId < eventId),
          Observable.timerRepeated(proxyConf.syncPolling, proxyConf.syncPolling, ())
        ).merge(implicitly[Observable[Any] <:< Observable[Any]]/*required for Scala 3???*/)
          .dropWhile(_ => currentState.eventId < eventId)
          .headL
          .void
    }

  /** For testing: wait for a condition in the running event stream. **/
  def when(predicate: EventAndState[Event, S] => Boolean): Task[EventAndState[Event, S]] =
    observable
      .filter(predicate)
      .headOptionL
      .map(_.getOrElse(throw new EndOfEventStreamException))

  final def stop: Task[Unit] =
    Task.deferFuture {
      observing.toOption.fold(Future.successful(())) { _ =>
        stopRequested.trySuccess(())
        observingStopped.future
      }
    }

  final def currentState: S =
    _currentState match {
      case null => throw new IllegalStateException("JournaledProxy has not yet started")
      case o => o
    }
}

object JournaledProxy
{
  private type RequiredApi_[S <: JournaledState[S]] =
    EventApi & HttpClusterNodeApi & SessionApi.HasUserAndPassword { type State = S }

  private val logger = scribe.Logger[this.type]

  def observable[S <: JournaledState[S]](
    apisResource: Resource[Task, Nel[RequiredApi_[S]]],
    fromEventId: Option[EventId],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    proxyConf: ProxyConf)
    (implicit S: JournaledState.Companion[S])
  : Observable[EventAndState[Event, S]] =
  {
    //if (apisResource.isEmpty) throw new IllegalArgumentException("apisResource must not be empty")

    def observable2: Observable[EventAndState[Event, S]] =
      Observable.tailRecM(none[S])(maybeState =>
        Observable
          .fromResource(
            ActiveClusterNodeSelector.selectActiveNodeApi[RequiredApi_[S]](
              apisResource,
              failureDelays = proxyConf.recouplingStreamReaderConf.failureDelays,
              _ => onCouplingError))
          .flatMap(api =>
            Observable
              .fromTask(
                durationOfTask(
                  maybeState.fold(api.checkedSnapshot(eventId = fromEventId))(s => Task.pure(Right(s)))))
              .map(o => o._1.orThrow/*TODO What happens then?*/ -> o._2)
              .flatMap { case (state, stateFetchDuration) =>
                var lastState = state
                observeWithState(api, state, stateFetchDuration)
                  .map { o =>
                    lastState = o.state
                    o
                  }
                  .pipe(obs => fromEventId.fold(obs)(
                    dropEventsUntilRequestedEventIdAndReinsertProxyStarted(obs, _)))
                  .map(Right.apply)
                  .onErrorRecoverWith {
                    case t if fromEventId.isEmpty || !isTorn(t) =>
                      val continueWithState =
                        if (isTorn(t)) {
                          logger.error(t.toStringWithCauses)
                          logger.warn("Restarting observation from a new snapshot, loosing some events")
                          None
                        } else {
                          logger.warn(t.toStringWithCauses)
                          if (t.getStackTrace.nonEmpty) logger.debug(t.toStringWithCauses, t)
                          logger.debug("Restarting observation and try to continue seamlessly after=" +
                            EventId.toString(state.eventId))
                          Some(lastState)
                        }
                      Observable.pure(Left(continueWithState))
                        .delayExecution(1.s/*TODO*/)
                  }
              }))

    def isTorn(t: Throwable) =
      fromEventId.isEmpty && checkedCast[ProblemException](t).exists(_.problem is EventSeqTornProblem)

    def observeWithState(api: RequiredApi_[S], state: S, stateFetchDuration: FiniteDuration)
    : Observable[EventAndState[Event, S]] = {
      val seed = EventAndState(Stamped(state.eventId, ProxyStarted: AnyKeyedEvent), state, state)
      val recouplingStreamReader = new MyRecouplingStreamReader(onProxyEvent, stateFetchDuration,
        tornOlder = (fromEventId.isEmpty ? proxyConf.tornOlder).flatten,
        proxyConf.recouplingStreamReaderConf)
      recouplingStreamReader.observe(api, after = state.eventId)
        .guarantee(recouplingStreamReader.decouple.map(_ => ()))
        .scan0(seed)((s, stampedEvent) =>
          EventAndState(stampedEvent,
            s.state,
            s.state.applyEvent(stampedEvent.value)
              .orThrow/*TODO Restart*/
              .withEventId(stampedEvent.eventId)))
    }

    def onCouplingError(throwable: Throwable) = Task {
      onProxyEvent(ProxyCouplingError(Problem.fromThrowable(throwable)))
    }

    observable2
      .tapEach(o => logger.trace(s"observable => ${o.stampedEvent.toString.truncateWithEllipsis(200)}"))
  }

  /** Drop all events until the requested one and
    * replace the first event by ProxyStarted.
    * The original ProxyStarted event may have been dropped.
    * Do this when returned snapshot is for an older EventId
    * (because it may be the original journal file snapshot).
    */
  private def dropEventsUntilRequestedEventIdAndReinsertProxyStarted[S <: JournaledState[S]](
    obs: Observable[EventAndState[Event, S]],
    fromEventId: EventId)
  : Observable[EventAndState[Event, S]] =
    // TODO Optimize this with SnapshotableStateBuilder ?
    obs.dropWhile(_.stampedEvent.eventId < fromEventId)
      .map {
        case es if es.stampedEvent.eventId == fromEventId &&
                   es.stampedEvent.value.event != ProxyStarted =>
          es.copy(
            stampedEvent = es.stampedEvent.copy(value = NoKey <-: ProxyStarted),
            previousState = es.state)
        case o => o
      }

  private class MyRecouplingStreamReader[S <: JournaledState[S]](
    onProxyEvent: ProxyEvent => Unit,
    stateFetchDuration: FiniteDuration,
    tornOlder: Option[FiniteDuration],
    recouplingStreamReaderConf: RecouplingStreamReaderConf)
    (implicit S: JournaledState.Companion[S])
  extends RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], RequiredApi_[S]](
    _.eventId, recouplingStreamReaderConf)
  {
    private var addToTornOlder = stateFetchDuration

    def getObservable(api: RequiredApi_[S], after: EventId) = {
      import S.keyedEventJsonCodec
      HttpClient.liftProblem(api
        .eventObservable(
          EventRequest.singleClass[Event](after = after, delay = 1.s,
            tornOlder = tornOlder.map(o => (o + addToTornOlder).roundUpToNext(100.ms)),
            timeout = Some(recouplingStreamReaderConf.timeout)))
        .doOnFinish {
          case None => Task { addToTornOlder = ZeroDuration }
          case _ => Task.unit
        })
    }

    override def onCoupled(api: RequiredApi_[S], after: EventId) =
      Task {
        onProxyEvent(ProxyCoupled(after))
        Completed
      }

    override protected def onCouplingFailed(api: RequiredApi_[S], problem: Problem) =
      super.onCouplingFailed(api, problem) >>
        Task {
          onProxyEvent(ProxyCouplingError(problem))
          false  // Terminate RecouplingStreamReader to allow to reselect a reachable Node (via Api[S[S)
        }

    override protected val onDecoupled =
      Task {
        onProxyEvent(ProxyDecoupled)
        Completed
      }

    def stopRequested = false
  }

  final class EndOfEventStreamException extends RuntimeException("Event stream terminated unexpectedly")
}
