package js7.proxy

import cats.effect.std.Supervisor
import cats.effect.{Deferred, IO, Resource, ResourceIO}
import fs2.Stream
import fs2.concurrent.Topic
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.service.Service
import js7.data.event.{Event, EventId, SnapshotableState}
import js7.proxy.JournaledProxy.*
import js7.proxy.JournaledProxyImpl.*
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.EventAndState

private final class JournaledProxyImpl[S <: SnapshotableState[S]] private[JournaledProxyImpl](
  underlyingStream: Stream[IO, EventAndState[Event, S]],
  proxyConf: ProxyConf,
  onEvent: EventAndState[Event, S] => Unit,
  topic: Topic[IO, EventAndState[Event, S]],
  supervisor: Supervisor[IO])
  (using S: SnapshotableState.Companion[S])
extends Service.StoppableByRequest, JournaledProxy[S]:

  @volatile private var _currentState: S = null.asInstanceOf[S]

  protected def start =
    Deferred[IO, Unit].flatMap: whenStateFetched =>
      supervisor
        .supervise:
          readAndPublishUnderlyingStream(whenStateFetched)
        .flatMap: fiber =>
          logger
            .debugIO("whenStateFetched"):
              // A started JournalProxy immediately provides `currentState: S`.
              // Wait until initial S has been read. This may take a long time !!!
              whenStateFetched.get
            .productR:
              startService:
                untilStopRequested
                  .guarantee:
                    fiber.cancel

  private def readAndPublishUnderlyingStream(whenStateFetched: Deferred[IO, Unit]): IO[Unit] =
    logger.traceIO:
      underlyingStream
        .evalTap(eventAndState => IO.defer:
          _currentState = eventAndState.state
          whenStateFetched.complete(()).void *>
            IO(onEvent(eventAndState)))
        .interruptWhen(untilStopRequested.attempt)
        .through(topic.publish)
        .compile
        .drain

  def subscribe(maxQueued: Option[Int] = None)
  : ResourceIO[Stream[IO, EventAndState[Event, S]]] =
    topic.subscribeAwait(maxQueued = maxQueued getOrElse proxyConf.eventQueueSize)

  def stream(queueSize: Option[Int] = None): Stream[IO, EventAndState[Event, S]] =
    logger.debugStream(s"Stream[IO, EventAndState[Event, $S]"):
      topic.subscribe(maxQueued = queueSize getOrElse proxyConf.eventQueueSize)

  def sync(eventId: EventId): IO[Unit] =
   logger.traceIO("sync", EventId.toString(eventId)):
    IO.defer:
      IO.unlessA(currentState.eventId >= eventId):
        subscribe().use:
          _.dropWhile(_.stampedEvent.eventId < eventId)
            .merge:
              Stream.fixedRateStartImmediately[IO](proxyConf.syncPolling)
            .dropWhile(_ => currentState.eventId < eventId)
            .head
            .compile.last
            .flatMap:
              case None => IO.raiseError(new NoSuchElementException(
                s"JournaledProxy#sync: Requested eventId=${EventId.toString(eventId)} not found"))
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

  override def toString = s"JournaledProxyImpl[$S]"


private object JournaledProxyImpl:

  private val logger = Logger[this.type]

  def resource[S <: SnapshotableState[S]](
    baseStream: Stream[IO, EventAndState[Event, S]],
    proxyConf: ProxyConf,
    onEvent: EventAndState[Event, S] => Unit)
    (using SnapshotableState.Companion[S])
  : ResourceIO[JournaledProxy[S]] =
    for
      supervisor <- Supervisor[IO]
      topic <- Resource.make(
        acquire = Topic[IO, EventAndState[Event, S]])(
        release = _.close.void)
      journaledProxy <- Service.resource(IO:
        new JournaledProxyImpl[S](baseStream, proxyConf, onEvent, topic, supervisor))
    yield
      journaledProxy
