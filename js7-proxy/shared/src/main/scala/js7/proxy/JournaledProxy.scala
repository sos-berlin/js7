package js7.proxy

import cats.effect.Resource
import js7.base.generic.Completed
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.web.HttpClient
import js7.common.http.RecouplingStreamReader
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.event.{AnyKeyedEvent, Event, EventApi, EventId, EventRequest, JournaledState, NoKeyEvent, Stamped}
import js7.proxy.ProxyEvent.{ProxyCoupled, ProxyCouplingError, ProxyDecoupled}
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.atomic.AtomicBoolean
import monix.reactive.Observable
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

final class JournaledProxy[S <: JournaledState[S]] private[proxy](
  observable: Observable[EventAndState[Event, S]],
  onEvent: EventAndState[Event, S] => Unit)
{
  private val observeCalled = AtomicBoolean(false)
  private val subscribed = AtomicBoolean(false)
  private val currentStateFilled = Promise[Unit]()
  @volatile private var _currentState: (EventId, S) = null
  private val observing = SetOnce[CancelableFuture[Unit]]("observing")
  private val observingStopped = Promise[Unit]()

  private[proxy] def startObserving: Task[Unit] =
    Task.deferFutureAction { implicit scheduler =>
      assertThat(observing.isEmpty)
      val obs = observe.completedL.runToFuture
      observing := obs
      obs.onComplete {
        case Success(()) =>
          scribe.error("Observable has terminated")
          // ???
        case Failure(t) =>
          scribe.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          // ???
      }
      whenStarted
    }

  def whenStarted: Future[Unit] =
    currentStateFilled.future

  def stop: Task[Unit] =
    Task.deferFuture {
      for (o <- observing) o.cancel()
      observingStopped.future
    }

  def observe: Observable[EventAndState[Event, S]] = {
    assertObserveNotCalled()
    observable
      .doOnSubscribe(Task {
        assertNotSubscribed()
      })
      .map { eventAndState =>
        _currentState = eventAndState.stampedEvent.eventId -> eventAndState.state
        currentStateFilled.trySuccess(())
        onEvent(eventAndState)
        eventAndState
      }
      .guarantee(Task {
        observingStopped.success(())
      })
  }

  def currentState: (EventId, S) =
    _currentState

  private def assertObserveNotCalled(): Unit =
    if (observeCalled.getAndSet(true)) throw new IllegalStateException("JournalProxy is already in use")

  private def assertNotSubscribed(): Unit =
    if (subscribed.getAndSet(true)) throw new IllegalStateException("JournalProxy is already in use")
}

object JournaledProxy
{
  type ApiResource = Resource[Task, EventApi]

  private val recouplingStreamReaderConf = RecouplingStreamReaderConf(timeout = 55.s, delay = 1.s)

  def start[S <: JournaledState[S]](
    apiResource: ApiResource,
    onProxyEvent: ProxyEvent => Unit,
    onEvent: EventAndState[Event, S] => Unit)
    (implicit S: JournaledState.Companion[S])
  : Task[JournaledProxy[S]] = {
    val proxy = apply[S](apiResource, onProxyEvent, onEvent)
    proxy.startObserving.map(_ => proxy)
  }

  def apply[S <: JournaledState[S]](
    apiResource: ApiResource,
    onProxyEvent: ProxyEvent => Unit,
    onEvent: EventAndState[Event, S] => Unit)
    (implicit S: JournaledState.Companion[S])
  : JournaledProxy[S] =
    new JournaledProxy(observable(apiResource, onProxyEvent), onEvent)

  def observable[S <: JournaledState[S]](
    apiResource: ApiResource,
    onProxyEvent: ProxyEvent => Unit)
    (implicit S: JournaledState.Companion[S])
  : Observable[EventAndState[Event, S]] =
  {
    import S.keyedEventJsonDecoder

    def observe: Observable[EventAndState[Event, S]] =
      Observable.fromResource(apiResource)
        .flatMap(api =>
          Observable.tailRecM(())(_ =>
            Observable
              .fromTask(
                api.loginUntilReachable(onError = onCouplingError) >>
                  api.retryUntilReachable(onError = onCouplingError)(api.snapshot)
                    .map(_.map(snapshot => snapshot.eventId -> S.fromIterable(snapshot.value).withEventId(snapshot.eventId))))
              .map(_.orThrow/*???*/)
              .flatMap { case (eventId, snapshotState) =>
                val seed = EventAndState(Stamped(eventId, ProxyStartedSeed: AnyKeyedEvent), snapshotState)
                val recouplingStreamReader = newRecouplingStreamReader()
                recouplingStreamReader.observe(api, after = eventId)
                  .scan0(seed)((s, stampedEvent) =>
                    EventAndState(stampedEvent, s.state.applyEvent(stampedEvent.value).orThrow/*TODO Restart*/))
                  .map(Right.apply)
                  .onErrorHandleWith { t =>
                    scribe.error(t.toStringWithCauses, t.nullIfNoStackTrace)
                    scribe.warn("Restarting observation from a new snapshot, loosing some events")
                    Observable.pure(Left(())/*TODO Observable.tailRecM: Left leaks memory, https://github.com/monix/monix/issues/791*/)
                      .delayExecution(1.s/*TODO*/)
                  }
                  .guarantee(recouplingStreamReader.decouple.map(_ => ()))
              }))

    def onCouplingError(throwable: Throwable) = Task {
      onProxyEvent(ProxyCouplingError(Problem.fromThrowable(throwable)))
      true
    }

    def newRecouplingStreamReader() =
      new RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], EventApi](_.eventId, recouplingStreamReaderConf)
      {
        def getObservable(api: EventApi, after: EventId) =
          HttpClient.liftProblem(
            api.eventObservable(
              EventRequest.singleClass[Event](after = after, delay = 50.ms, timeout = Some(55.s/*TODO*/))))

        override def onCoupled(api: EventApi, after: EventId) =
          Task {
            onProxyEvent(ProxyCoupled(after))
            Completed
          }

        override protected def onCouplingFailed(api: EventApi, problem: Problem) =
          super.onCouplingFailed(api, problem) >>
            Task {
              onProxyEvent(ProxyCouplingError(problem))
              Completed
            }

        override protected val onDecoupled =
          Task {
            onProxyEvent(ProxyDecoupled)
            Completed
          }

        def stopRequested = false
      }

    observe
  }

  private case object ProxyStartedSeed extends NoKeyEvent
}
