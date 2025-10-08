package js7.proxy

import cats.effect.{IO, ResourceIO}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.option.*
import fs2.Stream
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.{Problem, ProblemException}
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{DelayConf, Delayer}
import js7.base.web.HttpClient
import js7.cluster.watch.api.{ActiveClusterNodeSelector, HttpClusterNodeApi}
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.common.http.{PekkoHttpClient, RecouplingStreamReader}
import js7.data.Problems.OldEventIdProblem
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventApi, EventId, EventRequest, EventSeqTornProblem, JournaledState, SnapshotableState, Stamped}
import js7.data.problems.UnknownEventIdProblem
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.ProxyEvent.{ProxyCoupled, ProxyCouplingError, ProxyDecoupled}
import js7.proxy.data.event.{EventAndState, ProxyEvent, ProxyStarted}
import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.scalaUtilChainingOps

trait JournaledProxy[S <: SnapshotableState[S]]:

  def currentState: S

  def name: String

  def stream(queueSize: Option[Int] = None): Stream[IO, EventAndState[Event, S]]

  def subscribe(maxQueued: Option[Int] = None): ResourceIO[Stream[IO, EventAndState[Event, S]]]

  def sync(eventId: EventId): IO[Unit]

  /** For testing: wait for a condition in the running event stream. * */
  def when(predicate: EventAndState[Event, S] => Boolean): IO[EventAndState[Event, S]]


object JournaledProxy:
  private type RequiredApi_[S <: JournaledState[S]] =
    EventApi & HttpClusterNodeApi & SessionApi.HasUserAndPassword { type State = S }

  private val logger = Logger[this.type]

  def resource[S <: SnapshotableState[S]](
    baseStream: Stream[IO, EventAndState[Event, S]],
    proxyConf: ProxyConf,
    onEvent: EventAndState[Event, S] => Unit)
    (using SnapshotableState.Companion[S])
  : ResourceIO[JournaledProxy[S]] =
    JournaledProxyService.resource[S](baseStream, proxyConf, onEvent)

  def stream[S <: JournaledState[S]](
    apisResource: ResourceIO[Nel[RequiredApi_[S]]],
    fromEventId: Option[EventId],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    proxyConf: ProxyConf)
    (using S: JournaledState.Companion[S], sTag: Tag[S])
  : Stream[IO, EventAndState[Event, S]] =
    //if (apisResource.isEmpty) throw new IllegalArgumentException("apisResource must not be empty")

    def stream2(delayer: Delayer[IO]): Stream[IO, EventAndState[Event, S]] =
      none[S].tailRecM: maybeState =>
        Stream.resource:
          ActiveClusterNodeSelector.selectActiveNodeApi[RequiredApi_[S]](
            apisResource,
            proxyConf.recouplingStreamReaderConf.delayConf,
            problem => IO(onProxyEvent(ProxyCouplingError(problem))))
        .flatMap: api =>
          Stream.eval:
            maybeState.fold(api.checkedSnapshot(eventId = fromEventId))(IO.right).timed
          .map(o => o._1 -> o._2.orThrow /*!!!*/)
          .flatMap: (stateFetchDuration, state) =>
            var lastState = state
            streamWithState(api, state, stateFetchDuration)
              .map: o =>
                lastState = o.state
                o
              .pipe: obs =>
                fromEventId.fold(obs):
                  dropEventsUntilRequestedEventIdAndReinsertProxyStarted(obs, _)
              .map(Right.apply)
              .evalTap(_ => delayer.reset)
              .handleErrorWith: t =>
                if t.getClass.getName == "org.apache.pekko.stream.AbruptTerminationException"
                  || fromEventId.nonEmpty && isTorn(t)
                then
                  Stream.raiseError(t)
                else
                  Stream
                    .suspend(Stream.emit(Left:
                      if isTorn(t) then
                        logger.error(t.toStringWithCauses)
                        logger.warn("Restarting stream from a new snapshot, loosing some events")
                        None
                      else
                        logger.warn(t.toStringWithCauses)
                        if PekkoHttpClient.hasRelevantStackTrace(t) then
                          logger.debug(t.toStringWithCauses, t)
                        logger.debug("Restarting stream and try to continue seamlessly after=" +
                          EventId.toString(state.eventId))
                        Some(lastState)))
                    .evalTap(_ => delayer.sleep)

    def isTorn(t: Throwable) =
      t.checkedSubtype[ProblemException].map(_.problem)
        .exists: problem =>
          problem.is(OldEventIdProblem) || problem.is(UnknownEventIdProblem) ||
            problem.is(EventSeqTornProblem)

    def streamWithState(api: RequiredApi_[S], state: S, stateFetchDuration: FiniteDuration)
    : Stream[IO, EventAndState[Event, S]] =
      val seed = EventAndState(Stamped(state.eventId, ProxyStarted: AnyKeyedEvent), state, state)
      val recouplingStreamReader = new MyRecouplingStreamReader(onProxyEvent, stateFetchDuration,
        tornOlder = (fromEventId.isEmpty ? proxyConf.tornOlder).flatten,
        proxyConf.recouplingStreamReaderConf)
      recouplingStreamReader.stream(api, after = state.eventId)
        .onFinalize:
          recouplingStreamReader.decouple.void
        .scan(seed): (s, stampedEvent) =>
          EventAndState(stampedEvent,
            s.state,
            s.state.applyKeyedEvent(stampedEvent.value)
              .orThrow/*TODO Restart*/
              .withEventId(stampedEvent.eventId))

    Stream.eval:
      DelayConf(1.s, 1.s, 1.s, 1.s, 1.s, 2.s, 3.s, 5.s).start[IO]
    .flatMap:
      stream2
  end stream

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
    // TODO Optimize this with SnapshotableStateRecoverer ?
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
    extends
      RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], RequiredApi_[S]](
        _.eventId.some, recouplingStreamReaderConf):

    private var addToTornOlder = stateFetchDuration

    def getStream(api: RequiredApi_[S], after: EventId) =
      import S.keyedEventJsonCodec
      HttpClient.liftProblem:
        api.eventStream(
          EventRequest.singleClass[Event](
            after = after,
            timeout = None,
            tornOlder = tornOlder.map(o => (o + addToTornOlder).roundUpToNext(100.ms))),
          heartbeat = recouplingStreamReaderConf.keepAlive.some,
          idleTimeout = recouplingStreamReaderConf.timeout)
        .map:
          _.recoverWith(PekkoHttpClient.warnIdleTimeout)
        .flatTap(_ => IO:
          addToTornOlder = ZeroDuration)

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
