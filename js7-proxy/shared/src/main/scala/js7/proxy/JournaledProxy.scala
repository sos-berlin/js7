package js7.proxy

import cats.effect.std.Supervisor
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.option.*
import fs2.Stream
import fs2.concurrent.Topic
import js7.base.catsutils.CatsEffectExtensions.{joinStd, right}
import js7.base.catsutils.CatsEffectUtils
import js7.base.catsutils.CatsEffectUtils.durationOfIO
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Problem, ProblemException}
import js7.base.service.Service
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.*
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
import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.scalaUtilChainingOps

private final class JournaledProxy[S <: SnapshotableState[S]] private[JournaledProxy](
  inputStream: Stream[IO, EventAndState[Event, S]],
  onEvent: EventAndState[Event, S] => Unit,
  proxyConf: ProxyConf,
  topic: Topic[IO, EventAndState[Event, S]])
  (using S: SnapshotableState.Companion[S])
extends Service.StoppableByRequest:

  //private val observing = SetOnce[SyncCancelable]("connectableObservingCompleted")
  //private val observingStopped = Deferred.unsafe[IO, Unit]

  @volatile private var _currentState = S.empty

  protected def start =
    startService:
      val whenStarted = Deferred.unsafe[IO, Unit]
      Supervisor[IO](await = false).use: supervisor =>
        supervisor
          .supervise:
            readInputStream(whenStarted)
          .flatMap: fiber =>
            whenStarted.get *>
              untilStopRequested *>
              fiber.joinStd

  private def readInputStream(whenStarted: Deferred[IO, Unit]): IO[Unit] =
    inputStream
      .evalTap(eventAndState => IO.defer:
        _currentState = eventAndState.state
        whenStarted.complete(()))
      .map: eventAndState =>
        onEvent(eventAndState)
        eventAndState
      .interruptWhen(untilStopRequested.attempt)
      .through(topic.publish)
      .compile
      .drain

  //private def startObserving: IO[Unit] =
  //  IO.defer:
  //    val cancelable = SerialSyncCancelable()
  //    val stream = topic.subscribe(maxQueued = proxyConf.eventQueueSize)
  //    observing := stream
  //    cancelable := stream
  //    val whenCompleted = topic.completedL.unsafeToFuture()
  //    observingStopped.completeWith(whenCompleted)
  //    whenCompleted.onComplete:
  //      case Success(()) =>
  //        if !stopRequested.isCompleted then
  //          logger.error("Stream has terminated")
  //        // ???
  //      case Failure(t) =>
  //        logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
  //        // ???
  //    CancelableFuture(
  //      currentStateFilled.future,
  //      () =>
  //        logger.debug("startObserving: cancelling")
  //        whenCompleted.cancel()
  //        cancelable.cancel())

  def subscribe(maxQueued: Option[Int] = None)
  : ResourceIO[Stream[IO, EventAndState[Event, S]]] =
    topic.subscribeAwait(maxQueued = maxQueued getOrElse proxyConf.eventQueueSize)

  @deprecated("Prefer subscribe")
  def stream(maxQueued: Option[Int] = None): Stream[IO, EventAndState[Event, S]] =
    topic.subscribe(maxQueued = maxQueued getOrElse proxyConf.eventQueueSize)

  def sync(eventId: EventId): IO[Unit] =
    IO.defer:
      IO.unlessA(currentState.eventId >= eventId):
        subscribe().use:
          _.dropWhile(_.stampedEvent.eventId < eventId)
            .merge:
              Stream.fixedRateStartImmediately[IO](proxyConf.syncPolling)
            .dropWhile(_ => currentState.eventId < eventId)
            .compile
            .last
            .flatMap:
              case None => IO.raiseError(new NoSuchElementException:
                s"JournaledProxy#sync: Requested eventId=${EventId.toString(eventId)} not found")
              case Some(_) => IO.unit

  /** For testing: wait for a condition in the running event stream. * */
  def when(predicate: EventAndState[Event, S] => Boolean): IO[EventAndState[Event, S]] =
    subscribe().use:
      _.filter(predicate)
        .head.compile.last
        .map(_.getOrElse(throw new EndOfEventStreamException))

  def currentState: S =
    _currentState match
      case null => throw new IllegalStateException("JournaledProxy has not yet started")
      case o => o


object JournaledProxy:
  private type RequiredApi_[S <: JournaledState[S]] =
    EventApi & HttpClusterNodeApi & SessionApi.HasUserAndPassword { type State = S }

  private val logger = Logger[this.type]

  private[proxy] def resource[S <: SnapshotableState[S]](
    baseStream: Stream[IO, EventAndState[Event, S]],
    proxyConf: ProxyConf,
    onEvent: EventAndState[Event, S] => Unit)
    (using SnapshotableState.Companion[S])
  : ResourceIO[JournaledProxy[S]] =
    for
      topic <- Resource.make(
        acquire = Topic[IO, EventAndState[Event, S]])(
        release = _.close.void)
      journaledProxy <- Service.resource(IO:
        new JournaledProxy[S](baseStream, onEvent, proxyConf, topic))
    yield
      journaledProxy

  def stream[S <: JournaledState[S]](
    apisResource: ResourceIO[Nel[RequiredApi_[S]]],
    fromEventId: Option[EventId],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    proxyConf: ProxyConf)
    (using S: JournaledState.Companion[S])
  : Stream[IO, EventAndState[Event, S]] =
    //if (apisResource.isEmpty) throw new IllegalArgumentException("apisResource must not be empty")

    def stream2: Stream[IO, EventAndState[Event, S]] =
      none[S].tailRecM: maybeState =>
        Stream
          .resource:
            ActiveClusterNodeSelector.selectActiveNodeApi[RequiredApi_[S]](
              apisResource,
              failureDelays = proxyConf.recouplingStreamReaderConf.failureDelays,
              _ => onCouplingError)
          .flatMap: api =>
            Stream
              .eval:
                durationOfIO:
                  maybeState.fold(api.checkedSnapshot(eventId = fromEventId))(IO.right)
              .map(o => o._1.orThrow/*TODO What happens then?*/ -> o._2)
              .flatMap: (state, stateFetchDuration) =>
                var lastState = state
                streamWithState(api, state, stateFetchDuration)
                  .map: o =>
                    lastState = o.state
                    o
                  .pipe: obs =>
                    fromEventId.fold(obs):
                      dropEventsUntilRequestedEventIdAndReinsertProxyStarted(obs, _)
                  .map(Right.apply)
                  .recoverWith:
                    case t if fromEventId.isEmpty || !isTorn(t) =>
                      val continueWithState =
                        if isTorn(t) then
                          logger.error(t.toStringWithCauses)
                          logger.warn("Restarting observation from a new snapshot, loosing some events")
                          None
                        else
                          logger.warn(t.toStringWithCauses)
                          if t.getStackTrace.nonEmpty then logger.debug(t.toStringWithCauses, t)
                          logger.debug("Restarting observation and try to continue seamlessly after=" +
                            EventId.toString(state.eventId))
                          Some(lastState)
                      Stream.emit(Left(continueWithState))
                        .delayBy(1.s/*TODO*/)

    def isTorn(t: Throwable) =
      fromEventId.isEmpty && checkedCast[ProblemException](t).exists(_.problem is EventSeqTornProblem)

    def streamWithState(api: RequiredApi_[S], state: S, stateFetchDuration: FiniteDuration)
    : Stream[IO, EventAndState[Event, S]] =
      val seed = EventAndState(Stamped(state.eventId, ProxyStarted: AnyKeyedEvent), state, state)
      val recouplingStreamReader = new MyRecouplingStreamReader(onProxyEvent, stateFetchDuration,
        tornOlder = (fromEventId.isEmpty ? proxyConf.tornOlder).flatten,
        proxyConf.recouplingStreamReaderConf)
      recouplingStreamReader.observe(api, after = state.eventId)
        .onFinalize(recouplingStreamReader.decouple.map(_ => ()))
        .scan(seed)((s, stampedEvent) =>
          EventAndState(stampedEvent,
            s.state,
            s.state.applyEvent(stampedEvent.value)
              .orThrow/*TODO Restart*/
              .withEventId(stampedEvent.eventId)))

    def onCouplingError(throwable: Throwable) = IO:
      onProxyEvent(ProxyCouplingError(Problem.fromThrowable(throwable)))

    stream2
      .evalTap(o => IO:
        logger.trace(s"stream => ${o.stampedEvent.toString.truncateWithEllipsis(200)}"))

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
        .flatTap(_ => IO:
          addToTornOlder = ZeroDuration))


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


  trait Delegate[S <: SnapshotableState[S]]:
    protected val journaledProxy: JournaledProxy[S]

    def currentState: S =
      journaledProxy.currentState

    @deprecated("Use subscribe")
    def stream(maxQueued: Option[Int] = None): Stream[IO, EventAndState[Event, S]] =
      journaledProxy.stream(maxQueued = maxQueued)

    def subscribe(maxQueued: Option[Int] = None)
    : Resource[IO, Stream[IO, EventAndState[Event, S]]] =
      journaledProxy.subscribe(maxQueued = maxQueued)

    def sync(eventId: EventId): IO[Unit] =
      journaledProxy.sync(eventId)

    /** For testing: wait for a condition in the running event stream. * */
    def when(predicate: EventAndState[Event, S] => Boolean): IO[EventAndState[Event, S]] =
      journaledProxy.when(predicate)


  final class EndOfEventStreamException extends RuntimeException("Event stream terminated unexpectedly")
