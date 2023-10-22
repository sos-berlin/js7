package js7.proxy

import cats.effect.Resource
import cats.syntax.option.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.durationOfIO
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
import cats.effect.IO
import js7.base.monixlike.SerialCancelable
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import fs2.Stream
import monix.reactive.streams.ConnectableStream
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success}

trait JournaledProxy[S <: SnapshotableState[S]]:
  protected val baseStream: Stream[IO, EventAndState[Event, S]]
  protected def scheduler: Scheduler
  protected val onEvent: EventAndState[Event, S] => Unit
  protected def S: SnapshotableState.Companion[S]
  protected def proxyConf: ProxyConf

  private val observing = SetOnce[Cancelable]("connectableObservingCompleted")
  private val stopRequested = Promise[Unit]()
  private val observingStopped = Promise[Unit]()

  private val currentStateFilled = Promise[Unit]()
  @volatile private var _currentState = S.empty

  private val connectableStream: ConnectableStream[EventAndState[Event, S]] =
    baseStream
      .map { eventAndState =>
        _currentState = eventAndState.state
        currentStateFilled.trySuccess(())
        onEvent(eventAndState)
        eventAndState
      }
      .takeUntil(Stream.fromFuture(stopRequested.future))
      .doOnSubscriptionCancel(IO(
        logger.debug("connectableStream: cancelling")))
      .publish(scheduler)

  final def stream: Stream[IO, EventAndState[Event, S]] =
    connectableStream

  final def startObserving: IO[Unit] =
    val cancelable = SerialCancelable()
    IO.deferFutureAction { implicit scheduler =>
      assertThat(observing.isEmpty)
      val obs = connectableStream.connect()
      observing := obs
      cancelable := obs
      val whenCompleted = connectableStream.completedL.runToFuture
      observingStopped.completeWith(whenCompleted)
      whenCompleted.onComplete:
        case Success(()) =>
          if !stopRequested.isCompleted then
            logger.error("Stream has terminated")
          // ???
        case Failure(t) =>
          logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          // ???
      CancelableFuture(
        currentStateFilled.future,
        () => {
          logger.debug("startObserving: cancelling")
          whenCompleted.cancel()
          cancelable.cancel()
        })
    }

  def sync(eventId: EventId): IO[Unit] =
    IO.defer:
      if currentState.eventId >= eventId then
        IO.unit
      else
        Stream(
          stream.dropWhile(_.stampedEvent.eventId < eventId),
          Stream.timerRepeated(proxyConf.syncPolling, proxyConf.syncPolling, ())
        ).merge(implicitly[Stream[IO, Any] <:< Stream[IO, Any]]/*required for Scala 3???*/)
          .dropWhile(_ => currentState.eventId < eventId)
          .headL
          .void

  /** For testing: wait for a condition in the running event stream. **/
  def when(predicate: EventAndState[Event, S] => Boolean): IO[EventAndState[Event, S]] =
    stream
      .filter(predicate)
      .headOptionL
      .map(_.getOrElse(throw new EndOfEventStreamException))

  final def stop: IO[Unit] =
    IO.deferFuture:
      observing.toOption.fold(Future.successful(())) { _ =>
        stopRequested.trySuccess(())
        observingStopped.future
      }

  final def currentState: S =
    _currentState match
      case null => throw new IllegalStateException("JournaledProxy has not yet started")
      case o => o


object JournaledProxy:
  private type RequiredApi_[S <: JournaledState[S]] =
    EventApi & HttpClusterNodeApi & SessionApi.HasUserAndPassword { type State = S }

  private val logger = Logger[this.type]

  def stream[S <: JournaledState[S]](
    apisResource: Resource[IO, Nel[RequiredApi_[S]]],
    fromEventId: Option[EventId],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    proxyConf: ProxyConf)
    (implicit S: JournaledState.Companion[S])
  : Stream[IO, EventAndState[Event, S]] =
    //if (apisResource.isEmpty) throw new IllegalArgumentException("apisResource must not be empty")

    def stream2: Stream[IO, EventAndState[Event, S]] =
      Stream.tailRecM(none[S])(maybeState =>
        Stream
          .resource(
            ActiveClusterNodeSelector.selectActiveNodeApi[RequiredApi_[S]](
              apisResource,
              failureDelays = proxyConf.recouplingStreamReaderConf.failureDelays,
              _ => onCouplingError))
          .flatMap(api =>
            Stream
              .fromIO(
                durationOfIO(
                  maybeState.fold(api.checkedSnapshot(eventId = fromEventId))(s => IO.pure(Right(s)))))
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
                        if isTorn(t) then {
                          logger.error(t.toStringWithCauses)
                          logger.warn("Restarting observation from a new snapshot, loosing some events")
                          None
                        } else {
                          logger.warn(t.toStringWithCauses)
                          if t.getStackTrace.nonEmpty then logger.debug(t.toStringWithCauses, t)
                          logger.debug("Restarting observation and try to continue seamlessly after=" +
                            EventId.toString(state.eventId))
                          Some(lastState)
                        }
                      Stream.emit(Left(continueWithState))
                        .delayBy(1.s/*TODO*/)
                  }
              }))

    def isTorn(t: Throwable) =
      fromEventId.isEmpty && checkedCast[ProblemException](t).exists(_.problem is EventSeqTornProblem)

    def observeWithState(api: RequiredApi_[S], state: S, stateFetchDuration: FiniteDuration)
    : Stream[IO, EventAndState[Event, S]] =
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

    def onCouplingError(throwable: Throwable) = IO:
      onProxyEvent(ProxyCouplingError(Problem.fromThrowable(throwable)))

    stream2
      .tapEach(o => logger.trace(s"stream => ${o.stampedEvent.toString.truncateWithEllipsis(200)}"))

  /** Drop all events until the requested one and
    * replace the first event by ProxyStarted.
    * The original ProxyStarted event may have been dropped.
    * Do this when returned snapshot is for an older EventId
    * (because it may be the original journal file snapshot).
    */
  private def dropEventsUntilRequestedEventIdAndReinsertProxyStarted[S <: JournaledState[S]](
    obs: Stream[IO, EventAndState[Event, S]],
    fromEventId: EventId)
  : Stream[IO, EventAndState[Event, S]] =
    // TODO Optimize this with SnapshotableStateBuilder ?
    obs.dropWhile(_.stampedEvent.eventId < fromEventId)
      .map:
        case es if es.stampedEvent.eventId == fromEventId &&
                   es.stampedEvent.value.event != ProxyStarted =>
          es.copy(
            stampedEvent = es.stampedEvent.copy(value = NoKey <-: ProxyStarted),
            previousState = es.state)
        case o => o

  private class MyRecouplingStreamReader[S <: JournaledState[S]](
    onProxyEvent: ProxyEvent => Unit,
    stateFetchDuration: FiniteDuration,
    tornOlder: Option[FiniteDuration],
    recouplingStreamReaderConf: RecouplingStreamReaderConf)
    (implicit S: JournaledState.Companion[S])
  extends RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], RequiredApi_[S]](
    _.eventId, recouplingStreamReaderConf):
    private var addToTornOlder = stateFetchDuration

    def getStream(api: RequiredApi_[S], after: EventId) =
      import S.keyedEventJsonCodec
      HttpClient.liftProblem(api
        .eventStream(
          EventRequest.singleClass[Event](after = after, delay = 1.s,
            tornOlder = tornOlder.map(o => (o + addToTornOlder).roundUpToNext(100.ms)),
            timeout = Some(recouplingStreamReaderConf.timeout)))
        .doOnFinish {
          case None => IO { addToTornOlder = ZeroDuration }
          case _ => IO.unit
        })

    override def onCoupled(api: RequiredApi_[S], after: EventId) =
      IO:
        onProxyEvent(ProxyCoupled(after))
        Completed

    override protected def onCouplingFailed(api: RequiredApi_[S], problem: Problem) =
      super.onCouplingFailed(api, problem) >>
        IO:
          onProxyEvent(ProxyCouplingError(problem))
          false  // Terminate RecouplingStreamReader to allow to reselect a reachable Node (via Api[S[S)

    override protected val onDecoupled =
      IO:
        onProxyEvent(ProxyDecoupled)
        Completed

    def stopRequested = false

  final class EndOfEventStreamException extends RuntimeException("Event stream terminated unexpectedly")
