package js7.cluster.watch.api

import cats.effect.{ExitCase, Resource}
import cats.syntax.traverse.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.data.cluster.{ClusterNodeApi, ClusterNodeState}
import monix.eval.{Fiber, Task}
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration

object ActiveClusterNodeSelector {
  private val logger = Logger[this.type]

  /** Selects the API for the active node, waiting until one is active.
   * The returned EventApi is logged-in. */
  final def selectActiveNodeApi[Api <: HttpClusterNodeApi](
    apisResource: Resource[Task, Nel[Api]],
    failureDelay: FiniteDuration,
    onCouplingError: Api => Throwable => Task[Unit] =
      (_: Api) => (t: Throwable) => SessionApi.onErrorTryAgain(toString, t).void)
  : Resource[Task, Api] =
    apisResource.flatMap(apis =>
      Resource.eval(
        selectActiveNodeApiOnly[Api](
          apis,
          api => throwable => onCouplingError(api)(throwable),
          failureDelay)))

  private def selectActiveNodeApiOnly[Api <: HttpClusterNodeApi](
    apis: Nel[Api],
    onCouplingError: Api => Throwable => Task[Unit],
    failureDelay: FiniteDuration)
  : Task[Api] =
    apis match {
      case Nel(api, Nil) =>
        api
          .loginUntilReachable(
            onError = t => onCouplingError(api)(t).as(true),
            onlyIfNotLoggedIn = true)
          .map((_: Completed) => api)

      case _ =>
        Task
          .tailRecM(())(_ => apis
            .traverse(api =>
              fetchClusterNodeState(api)
                .materializeIntoChecked  /*don't let the whole operation fail*/
                .start
                .map(ApiWithFiber(api, _)))
            .flatMap((apisWithClusterNodeStateFibers: Nel[ApiWithFiber[Api]]) =>
              Observable.fromIterable(apisWithClusterNodeStateFibers.toList)
                .map(o => Observable
                  .fromTask(o.fiber.join)
                  .map(ApiWithNodeState(o.api, _)))
                // Query nodes in parallel and continue with first response first
                .merge(implicitly[Observable[ApiWithNodeState[Api]] <:< Observable[ApiWithNodeState[Api]]]/*required for Scala 3???*/)
                .takeWhileInclusive(o => !o.clusterNodeState.exists(_.isActive))
                .toListL
                .flatMap { list =>
                  val maybeActive = list.lastOption collect {
                    case ApiWithNodeState(api, Right(nodeState)) if nodeState.isActive =>
                      api -> nodeState
                  }
                  logProblems(list, maybeActive, n = apis.length)
                  apisWithClusterNodeStateFibers
                    .collect { case o if list.forall(_.api ne o.api) =>
                      logger.trace(s"Cancel discarded request to ${o.api.baseUri}")
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
                    logger.debug(s"selectActiveNodeApiOnly => $exitCase")
                    apisWithClusterNodeStateFibers
                      .map(_.fiber.cancel)
                      .sequence
                      .as(())
                }))
          .onErrorRestartLoop(()) { (throwable, _, tryAgain) =>
            logger.warn(throwable.toStringWithCauses)
            if (throwable.getStackTrace.nonEmpty) logger.debug(s"💥 $throwable", throwable)
            tryAgain(()).delayExecution(failureDelay)
      }
      .map { case (api, clusterNodeState) =>
        val x = if (clusterNodeState.isActive) "active" else "maybe passive"
        logger.info(s"Selected $x ${clusterNodeState.nodeId} ${api.baseUri}")
        api
      }
    }

  private case class ApiWithFiber[Api <: HttpClusterNodeApi](
    api: Api,
    fiber: Fiber[Checked[ClusterNodeState]])

  private case class ApiWithNodeState[Api <: HttpClusterNodeApi](
    api: Api,
    clusterNodeState: Checked[ClusterNodeState])

  private def fetchClusterNodeState(api: HttpClusterNodeApi): Task[Checked[ClusterNodeState]] =
    HttpClient
      .liftProblem(
        api.retryIfSessionLost()(
          api.clusterNodeState))
      .tapEval(checked => Task(
        logger.trace(s"${api.baseUri} => ${checked.fold(identity, identity)}")))

  private def logProblems[Api <: HttpClusterNodeApi](
    list: List[ApiWithNodeState[Api]],
    maybeActive: Option[(ClusterNodeApi, ClusterNodeState)],
    n: Int)
  : Unit = {
    list.foreach {
      case ApiWithNodeState(api, Left(problem)) =>
        logger.warn(s"Cluster node ${api.baseUri} is not accessible: $problem")
      case _ =>
    }
    // Different clusterStates only iff nodes are not coupled
    val clusterStates = list
      .collect {
        case ApiWithNodeState(api, Right(clusterNodeState)) =>
          s"${api.baseUri} => ${clusterNodeState.clusterState.getClass.simpleScalaName}"
      }
      .mkString(" · ")
    if (maybeActive.isEmpty) logger.warn(
      s"No cluster node (out of $n) seems to be active${clusterStates.nonEmpty ?? s": $clusterStates"}")
  }
}
