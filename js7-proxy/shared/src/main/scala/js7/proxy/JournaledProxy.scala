package js7.proxy

import cats.effect.Resource
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.traverse._
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.durationOfTask
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Problem, ProblemException}
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.web.HttpClient
import js7.common.http.RecouplingStreamReader
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.event.{AnyKeyedEvent, Event, EventApi, EventId, EventRequest, EventSeqTornProblem, JournaledState, NoKeyEvent, Stamped}
import js7.proxy.ProxyEvent.{ProxyCoupled, ProxyCouplingError, ProxyDecoupled}
import js7.proxy.configuration.ProxyConf
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import scala.collection.immutable.Seq
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Future, Promise}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

trait JournaledProxy[S <: JournaledState[S]]
{
  protected val baseObservable: Observable[EventAndState[Event, S]]
  protected def scheduler: Scheduler
  protected val onEvent: EventAndState[Event, S] => Unit
  protected def S: JournaledState.Companion[S]

  private val observing = SetOnce[Cancelable]("connectableObservingCompleted")
  private val stopRequested = Promise[Unit]()
  private val observingStopped = Promise[Unit]()

  private val currentStateFilled = Promise[Unit]()
  @volatile private var _currentState = S.empty

  final val connectableObservable: ConnectableObservable[EventAndState[Event, S]] = {
    baseObservable
      .map { eventAndState =>
        _currentState = eventAndState.state
        currentStateFilled.trySuccess(())
        onEvent(eventAndState)
        eventAndState
      }
      .takeUntil(Observable.fromFuture(stopRequested.future))
      .publish(scheduler)
  }

  final def startObserving: Task[Unit] =
    Task.deferFutureAction { implicit scheduler =>
      assertThat(observing.isEmpty)
      val obs = connectableObservable.connect()
      observing := obs
      val whenCompleted = connectableObservable.completedL.runToFuture
      observingStopped.completeWith(whenCompleted)
      whenCompleted.onComplete {
        case Success(()) =>
          if (!stopRequested.isCompleted) {
            scribe.error("Observable has terminated")
          }
          // ???
        case Failure(t) =>
          scribe.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          // ???
      }
      whenStarted
    }

  final def whenStarted: Future[Unit] =
    currentStateFilled.future

  final def stop: Task[Unit] =
    Task.deferFuture {
      observing.toOption.fold(Future.successful(())) { _ =>
        stopRequested.trySuccess(())
        observingStopped.future
      }
    }

  final def observable: Observable[EventAndState[Event, S]] =
    connectableObservable

  final def currentState: S =
    _currentState match {
      case null => throw new IllegalStateException("JournaledProxy has not yet started")
      case o => o
    }
}

object JournaledProxy
{
  type ApiResource = Resource[Task, EventApi]

  private val recouplingStreamReaderConf = RecouplingStreamReaderConf(timeout = 55.s, delay = 1.s)

  def observable[S <: JournaledState[S]](
    apiResources: Seq[ApiResource],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    proxyConf: ProxyConf)
    (implicit S: JournaledState.Companion[S])
  : Observable[EventAndState[Event, S]] =
  {
    if (apiResources.isEmpty) throw new IllegalArgumentException("apiResources must not be empty")

    def observe: Observable[EventAndState[Event, S]] =
      Observable.fromResource(apiResources.toVector.sequence)
        .flatMap { apis =>
          Observable.tailRecM(none[S])(maybeState =>
            Observable.fromTask(selectActiveNodeApi(apis, _ => onCouplingError)).flatMap(api =>
              Observable
                .fromTask(
                  durationOfTask(
                    maybeState.fold(api.snapshotAs[S])(s => Task.pure(Right(s)))))
                .map(o => o._1.orThrow/*TODO What happens then?*/ -> o._2)
                .flatMap { case (state, stateFetchDuration) =>
                  var lastState = state
                  observeWithState(api, state, stateFetchDuration)
                    .map { o =>
                      lastState = o.state
                      o
                    }
                    .map(Right.apply)
                    .onErrorHandleWith { t =>
                      val continueWithState = t match {
                        case t: ProblemException if t.problem is EventSeqTornProblem =>
                          scribe.error(t.toStringWithCauses)
                          scribe.warn("Restarting observation from a new snapshot, loosing some events")
                          None
                        case t =>
                          scribe.warn(t.toStringWithCauses)
                          if (t.getStackTrace.nonEmpty) scribe.debug(t.toStringWithCauses, t.nullIfNoStackTrace)
                          scribe.debug(s"Restarting observation and try to continue seamlessly after=${EventId.toString(state.eventId)}")
                          Some(lastState)
                      }
                      Observable.pure(Left(continueWithState)/*TODO Observable.tailRecM: Left leaks memory, https://github.com/monix/monix/issues/791*/)
                        .delayExecution(1.s/*TODO*/)
                    }
                }))
        }

    def observeWithState(api: EventApi, state: S, stateFetchDuration: FiniteDuration): Observable[EventAndState[Event, S]] = {
      val seed = EventAndState(Stamped(state.eventId, ProxyStartedSeed: AnyKeyedEvent), state)
      val recouplingStreamReader = new MyRecouplingStreamReader(onProxyEvent, stateFetchDuration, proxyConf)
      recouplingStreamReader.observe(api, after = state.eventId)
        .guarantee(recouplingStreamReader.decouple.map(_ => ()))
        .scan0(seed)((s, stampedEvent) =>
          EventAndState(stampedEvent,
            s.state.applyEvent(stampedEvent.value)
              .orThrow/*TODO Restart*/
              .withEventId(stampedEvent.eventId)))
    }

    def onCouplingError(throwable: Throwable) = Task {
      onProxyEvent(ProxyCouplingError(Problem.fromThrowable(throwable)))
      true
    }

    observe
  }

  private class MyRecouplingStreamReader[S <: JournaledState[S]](onProxyEvent: ProxyEvent => Unit,
    stateFetchDuration: FiniteDuration, proxyConf: ProxyConf)
    (implicit S: JournaledState.Companion[S])
  extends RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], EventApi](_.eventId, recouplingStreamReaderConf)
  {
    private var addToTornOlder = stateFetchDuration

    def getObservable(api: EventApi, after: EventId) = {
      import S.keyedEventJsonDecoder
      HttpClient.liftProblem(
        api.eventObservable(
          EventRequest.singleClass[Event](after = after, delay = 1.s,
            tornOlder = proxyConf.tornOlder.map(o => (o + addToTornOlder).roundUpToNext(100.ms)),
            timeout = Some(55.s/*TODO*/)))
      .doOnFinish {
        case None => Task { addToTornOlder = Duration.Zero }
        case _ => Task.unit
      })
    }

    override def onCoupled(api: EventApi, after: EventId) =
      Task {
        onProxyEvent(ProxyCoupled(after))
        Completed
      }

    override protected def onCouplingFailed(api: EventApi, problem: Problem) =
      super.onCouplingFailed(api, problem) >>
        Task {
          onProxyEvent(ProxyCouplingError(problem))
          false  // Terminate RecouplingStreamReader to allow to reselect a reachable Node (via EventApi)
        }

    override protected val onDecoupled =
      Task {
        onProxyEvent(ProxyDecoupled)
        Completed
      }

    def stopRequested = false
  }

  /** Selects the API for the active node's, waiting until one is active.
    * The return EventApi is logged-in.
    * @param onCouplingError Boolean return value is ignored, will always continue trying
    * @return (EventApi, None) iff apis.size == 1
    *         (EventApi, Some(NodeId)) iff apis.size > 1
    */
  def selectActiveNodeApi[Api <: EventApi](
    apis: Seq[Api],
    onCouplingError: EventApi => Throwable => Task[Boolean/*ignored*/])
  : Task[Api] = {
    def onCouplingErrorThenContinue(api: EventApi)(t: Throwable): Task[Boolean] =
      onCouplingError(api)(t).map(_ => true)
    apis match {
      case Seq(api) =>
        api.loginUntilReachable(onError = onCouplingErrorThenContinue(api), onlyIfNotLoggedIn = true)
          .map((_: Completed) => api)

      case _ =>
        Task.deferAction { implicit s =>
          // Query nodes in parallel
          val apisAndFutures = apis.map { api =>
            api ->
              api.retryUntilReachable(onError = onCouplingErrorThenContinue(api))(
                api.clusterNodeState
              ) .materializeIntoChecked
                .onCancelRaiseError(CancelledException)
                .runToFuture
          }
          Observable
            .fromIterable(apisAndFutures map { case (api, future) =>
              Observable.fromFuture(future).map(api -> _)
            })
            .merge
            .takeWhileInclusive(o => !o._2.exists(_.isActive))
            .toL(Vector)
            .flatMap { seq =>
              val maybeActive = seq.lastOption match {
                case Some((api, Right(clusterNodeState))) if clusterNodeState.isActive =>
                  Some(api -> clusterNodeState)
                case _ =>
                  seq collectFirst { case (api, Right(clusterNodeState)) => api -> clusterNodeState }
              }

              val logoutOthers = apisAndFutures
                .filter { case (api, _) => maybeActive.forall(_._1 ne api) }
                .toList
                .traverse { case (api, future) =>
                  future.cancel()
                  Task.fromFuture(future).void.onErrorRecover { case CancelledException => } >>
                    Task(scribe.debug(s"Logout discard $api")) >>
                    api.logout()
                }
                .map(_.combineAll)

              logoutOthers.map((_: Completed) => maybeActive)
            }
            .map(_.toChecked(Problem.pure("No cluster node seems to be active")))
            .map(_.orThrow)
            .onErrorRestartLoop(()) { (throwable, _, tryAgain) =>
              scribe.warn(throwable.toStringWithCauses)
              if (throwable.getStackTrace.nonEmpty) scribe.debug(throwable.toString, throwable)
              tryAgain(()).delayExecution(5.s/*TODO*/)
            }
            .map { case (api, clusterNodeState) =>
              val x = if (clusterNodeState.isActive) "active" else "maybe passive"
              scribe.info(s"Coupling with $x node ${api.baseUri} '${clusterNodeState.nodeId}'")
              api
            }
      }
    }
  }

  private case object ProxyStartedSeed extends NoKeyEvent

  private case object CancelledException extends NoStackTrace
}
