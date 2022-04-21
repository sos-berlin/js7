package js7.proxy

import cats.effect.{ExitCase, Resource}
import cats.syntax.option._
import cats.syntax.traverse._
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.durationOfTask
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.session.SessionApi
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.SetOnce
import js7.base.web.HttpClient
import js7.common.http.RecouplingStreamReader
import js7.common.http.configuration.RecouplingStreamReaderConf
import js7.data.cluster.ClusterNodeState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventApi, EventId, EventRequest, EventSeqTornProblem, JournaledState, SnapshotableState, Stamped}
import js7.proxy.JournaledProxy._
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.ProxyEvent.{ProxyCoupled, ProxyCouplingError, ProxyDecoupled}
import js7.proxy.data.event.{EventAndState, ProxyEvent, ProxyStarted}
import monix.eval.{Fiber, Task}
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.observables.ConnectableObservable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NoStackTrace
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

  protected final def startObserving: Task[Unit] = {
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
        ) .merge
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
  private type Api_[S <: JournaledState[S]] =
    EventApi with SessionApi.HasUserAndPassword {type State = S}

  private val logger = scribe.Logger[this.type]

  def observable[S <: JournaledState[S]](
    apiResources: Seq[Resource[Task, Api_[S]]],
    fromEventId: Option[EventId],
    onProxyEvent: ProxyEvent => Unit = _ => (),
    proxyConf: ProxyConf)
    (implicit S: JournaledState.Companion[S])
  : Observable[EventAndState[Event, S]] =
  {
    if (apiResources.isEmpty) throw new IllegalArgumentException("apiResources must not be empty")

    def observable2: Observable[EventAndState[Event, S]] =
      Observable.tailRecM(none[S])(maybeState =>
        Observable.fromResource(selectActiveNodeApi(apiResources, _ => onCouplingError, proxyConf))
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
                      // FIXME Observable.tailRecM: Left leaks memory, https://github.com/monix/monix/issues/791
                      Observable.pure(Left(continueWithState))
                        .delayExecution(1.s/*TODO*/)
                  }
              }))

    def isTorn(t: Throwable) =
      fromEventId.isEmpty && checkedCast[ProblemException](t).exists(_.problem is EventSeqTornProblem)

    def observeWithState(api: Api_[S], state: S, stateFetchDuration: FiniteDuration)
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
  extends RecouplingStreamReader[EventId, Stamped[AnyKeyedEvent], Api_[S]](
    _.eventId, recouplingStreamReaderConf)
  {
    private var addToTornOlder = stateFetchDuration

    def getObservable(api: Api_[S], after: EventId) = {
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

    override def onCoupled(api: Api_[S], after: EventId) =
      Task {
        onProxyEvent(ProxyCoupled(after))
        Completed
      }

    override protected def onCouplingFailed(api: Api_[S], problem: Problem) =
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

  /** Selects the API for the active node, waiting until one is active.
    * The returned EventApi is logged-in.
    * @param onCouplingError Boolean return value is ignored, will always continue trying
    * @return (EventApi, None) iff apis.size == 1
    *         (EventApi, Some(NodeId)) iff apis.size > 1
    */
  final def selectActiveNodeApi[Api <: EventApi with SessionApi.HasUserAndPassword](
    apiResources: Seq[Resource[Task, Api]],
    onCouplingError: EventApi => Throwable => Task[Unit],
    proxyConf: ProxyConf)
  : Resource[Task, Api] =
    apiResources.sequence.flatMap(apis =>
      Resource.eval(
        selectActiveNodeApiOnly(
          apis,
          (api: EventApi) => throwable => onCouplingError(api)(throwable),
          proxyConf)))

  private def selectActiveNodeApiOnly[Api <: EventApi with SessionApi.HasUserAndPassword](
    apis: Seq[Api],
    onCouplingError: Api => Throwable => Task[Unit],
    proxyConf: ProxyConf)
  : Task[Api] =
    apis match {
      case Seq(api) =>
        api
          .loginUntilReachable(
            onError = t => onCouplingError(api)(t).as(true),
            onlyIfNotLoggedIn = true)
          .map((_: Completed) => api)

      case _ =>
        import proxyConf.recouplingStreamReaderConf.failureDelay
        Task.tailRecM(())(_ =>
          apis
            .traverse(api =>
              fetchClusterNodeState(api, onCouplingError(api))
                .materializeIntoChecked  /*don't let the whole operation fail*/
                .start
                .map(ApiWithFiber(api, _)))
            .flatMap(apisWithClusterNodeStateFibers =>
              Observable.fromIterable(apisWithClusterNodeStateFibers)
                .map(o => Observable.fromTask(o.fiber.join).map(ApiWithNodeState(o.api, _)))
                .merge /*query nodes in parallel and continue with first response first*/
                .takeWhileInclusive(o => !o.clusterNodeState.forall(_.isActive))
                .toListL
                .flatMap { list =>
                  val maybeActive = list.lastOption collect {
                    case ApiWithNodeState(api, Right(nodeState)) if nodeState.isActive =>
                      api -> nodeState
                  }
                  logProblems(list, maybeActive)
                  apisWithClusterNodeStateFibers
                    .collect { case o if list.forall(_.api ne o.api) =>
                      logger.debug(s"Cancel discarded request to '${o.api}'")
                      o.fiber.cancel
                    }
                    .sequence
                    .flatMap(_ => maybeActive match {
                      case None => Task.sleep(failureDelay).as(Left(()))
                      case Some(x) => Task.pure(Right(x))
                    })
                }
                .guaranteeCase {
                  case ExitCase.Completed => Task.unit
                  case exitCase =>
                    logger.debug(exitCase.toString)
                    apisWithClusterNodeStateFibers
                      .map(_.fiber.cancel)
                      .sequence
                      .map(_ => ())
                })
        )
        .onErrorRestartLoop(()) { (throwable, _, tryAgain) =>
          logger.warn(throwable.toStringWithCauses)
          if (throwable.getStackTrace.nonEmpty) logger.debug(throwable.toString, throwable)
          tryAgain(()).delayExecution(failureDelay)
      }
      .map { case (api, clusterNodeState) =>
        val x = if (clusterNodeState.isActive) "active" else "maybe passive"
        logger.info(s"Selected $x node ${api.baseUri} '${clusterNodeState.nodeId}'")
        api
      }
    }

  private case class ApiWithFiber[Api <: EventApi with SessionApi.HasUserAndPassword](
    api: Api,
    fiber: Fiber[Checked[ClusterNodeState]])

  private case class ApiWithNodeState[Api <: EventApi with SessionApi.HasUserAndPassword](
    api: Api,
    clusterNodeState: Checked[ClusterNodeState])

  private def fetchClusterNodeState[Api <: EventApi with SessionApi.HasUserAndPassword](api: Api, onError: Throwable => Task[Unit])
  : Task[Checked[ClusterNodeState]] =
    HttpClient
      .liftProblem(
        api.retryIfSessionLost(onError)(
          api.clusterNodeState))

  private def logProblems[Api <: EventApi with SessionApi.HasUserAndPassword](
    list: List[ApiWithNodeState[Api]],
    maybeActive: Option[(Api, ClusterNodeState)])
  : Unit = {
    list.collect { case ApiWithNodeState(api, Left(problem)) => api -> problem }
      .foreach { case (api, problem) => logger.warn(
        s"Cluster node '${api.baseUri}' is not accessible: $problem")
    }
    if (maybeActive.isEmpty) logger.warn("No cluster node seems to be active")
  }

  private case class InternalProblemException(problem: Problem) extends NoStackTrace {
    override def toString = problem.toString
  }

  final class EndOfEventStreamException extends RuntimeException("Event stream terminated unexpectedly")
}
