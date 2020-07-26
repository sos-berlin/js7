package js7.proxy

import cats.effect.Resource
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.traverse._
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.ifCast
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.common.http.RecouplingStreamReader
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.event.{AnyKeyedEvent, Event, EventApi, EventId, EventRequest, EventSeqTornProblem, JournaledState, NoKeyEvent, Stamped}
import js7.proxy.ProxyEvent.{ProxyCoupled, ProxyCouplingError, ProxyDecoupled}
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.atomic.AtomicBoolean
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

trait JournaledProxy[S <: JournaledState[S]]
{
  protected val observable: Observable[EventAndState[Event, S]]

  protected val onEvent: EventAndState[Event, S] => Unit

  private val observeCalled = AtomicBoolean(false)
  private val subscribed = AtomicBoolean(false)
  private val currentStateFilled = Promise[Unit]()
  @volatile private var _currentState: (EventId, S) = null
  private val observing = SetOnce[CancelableFuture[Unit]]("observing")
  private val observingStopped = Promise[Unit]()

  private[proxy] final def startObserving: Task[Unit] =
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

  final def whenStarted: Future[Unit] =
    currentStateFilled.future

  final def stop: Task[Unit] =
    Task.deferFuture {
      observing.toOption.fold(Future.successful(())) { observing =>
        observing.cancel()
        observingStopped.future
      }
    }

  final def observe: Observable[EventAndState[Event, S]] = {
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

  final def currentState: (EventId, S) =
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

  def observable[S <: JournaledState[S]](
    apiResources: Seq[ApiResource],
    onProxyEvent: ProxyEvent => Unit,
    tornOlder: Option[FiniteDuration] = None)
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
                  maybeState.fold(api.snapshotAs[S])(s => Task.pure(Right(s))))
                .map(_.orThrow/*TODO What happens then?*/)
                .flatMap(state =>
                  observeWithState(api, state)
                    .map(Right.apply)
                    .onErrorHandleWith { t =>
                      val continueWithState =
                        if (ifCast[HttpException](t).flatMap(_.problem).exists(_ is EventSeqTornProblem)) {
                          scribe.error(t.toStringWithCauses)
                          scribe.warn("Restarting observation from a new snapshot, loosing some events")
                          None
                        } else {
                          scribe.warn(t.toStringWithCauses, t.nullIfNoStackTrace)
                          scribe.debug("Restarting observation and try to continue seamlessly")
                          Some(state)
                        }
                      Observable.pure(Left(continueWithState)/*TODO Observable.tailRecM: Left leaks memory, https://github.com/monix/monix/issues/791*/)
                        .delayExecution(1.s/*TODO*/)
                    })))
        }

    def observeWithState(api: EventApi, state: S): Observable[EventAndState[Event, S]] = {
      val seed = EventAndState(Stamped(state.eventId, ProxyStartedSeed: AnyKeyedEvent), state)
      val recouplingStreamReader = new MyRecouplingStreamReader(onProxyEvent, tornOlder)
      recouplingStreamReader.observe(api, after = state.eventId)
        .guarantee(recouplingStreamReader.decouple.map(_ => ()))
        .scan0(seed)((s, stampedEvent) =>
          EventAndState(stampedEvent, s.state.applyEvent(stampedEvent.value).orThrow/*TODO Restart*/))
    }

    def onCouplingError(throwable: Throwable) = Task {
      onProxyEvent(ProxyCouplingError(Problem.fromThrowable(throwable)))
      true
    }

    observe
  }

  private class MyRecouplingStreamReader[S <: JournaledState[S]](onProxyEvent: ProxyEvent => Unit, tornOlder: Option[FiniteDuration])
    (implicit S: JournaledState.Companion[S])
    extends RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], EventApi](_.eventId, recouplingStreamReaderConf)
    {
      def getObservable(api: EventApi, after: EventId) = {
        import S.keyedEventJsonDecoder
        HttpClient.liftProblem(
          api.eventObservable(
            EventRequest.singleClass[Event](after = after, delay = 1.s, tornOlder = tornOlder, timeout = Some(55.s/*TODO*/))))
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
            .toListL
            .flatMap { list =>
              val maybeActive = list.lastOption match {
                case Some((api, Right(clusterNodeState))) if clusterNodeState.isActive =>
                  Some(api -> clusterNodeState)
                case _ =>
                  list collectFirst { case (api, Right(clusterNodeState)) => api -> clusterNodeState }
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
              for (t <- throwable.ifNoStackTrace) scribe.debug(t.toString, t)
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
