package js7.cluster.watch.api

import cats.effect.{ExitCase, Resource}
import cats.syntax.traverse.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.utils.CatsUtils.{Nel, continueWithLast}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.data.cluster.{ClusterNodeApi, ClusterNodeState}
import cats.effect.IO
import cats.effect.Fiber
import fs2.Stream
import scala.concurrent.duration.FiniteDuration

object ActiveClusterNodeSelector:
  private val logger = Logger[this.type]

  /** Selects the API for the active node, waiting until one is active.
   * The returned EventApi is logged-in. */
  final def selectActiveNodeApi[Api <: HttpClusterNodeApi](
    apisResource: Resource[IO, Nel[Api]],
    failureDelays: Nel[FiniteDuration],
    onCouplingError: Api => Throwable => IO[Unit] =
      (_: Api) => (t: Throwable) => SessionApi.onErrorTryAgain(toString, t).void)
  : Resource[IO, Api] =
    apisResource.flatMap(apis =>
      Resource.eval(
        selectActiveNodeApiOnly[Api](
          apis,
          api => throwable => onCouplingError(api)(throwable),
          failureDelays)))

  private def selectActiveNodeApiOnly[Api <: HttpClusterNodeApi](
    apis: Nel[Api],
    onCouplingError: Api => Throwable => IO[Unit],
    failureDelays: Nel[FiniteDuration])
  : IO[Api] =
    apis match
      case Nel(api, Nil) =>
        api
          .loginUntilReachable(
            onError = t => onCouplingError(api)(t).as(true),
            onlyIfNotLoggedIn = true)
          .map((_: Completed) => api)

      case _ =>
        IO.defer:
          val failureDelayIterator = continueWithLast(failureDelays)
          IO
            .tailRecM(())(_ => apis
              .traverse(api =>
                fetchClusterNodeState(api)
                  .materializeIntoChecked  /*don't let the whole operation fail*/
                  .start
                  .map(ApiWithFiber(api, _)))
              .flatMap((apisWithClusterNodeStateFibers: Nel[ApiWithFiber[Api]]) =>
                Stream.fromIterable(apisWithClusterNodeStateFibers.toList)
                  .map(o => Stream
                    .fromIO(o.fiber.join)
                    .map(ApiWithNodeState(o.api, _)))
                  // Query nodes in parallel and continue with first response first
                  .merge(implicitly[Stream[IO, ApiWithNodeState[Api]] <:< Stream[IO, ApiWithNodeState[Api]]]/*required for Scala 3???*/)
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
                        case None => IO.sleep(failureDelayIterator.next()).as(Left(()))
                        case Some(x) => IO.pure(Right(x))
                      })
                  }
                  .guaranteeCase {
                    case ExitCase.Completed => IO.unit
                    case exitCase =>
                      logger.debug(s"selectActiveNodeApiOnly => $exitCase")
                      apisWithClusterNodeStateFibers
                        .map(_.fiber.cancel)
                        .sequence
                        .as(())
                  }))
            .onErrorRestartLoop(()) { (throwable, _, tryAgain) =>
              logger.warn(throwable.toStringWithCauses)
              if throwable.getStackTrace.nonEmpty then logger.debug(s"💥 $throwable", throwable)
              tryAgain(()).delayBy(failureDelayIterator.next())
            }
            .map { case (api, clusterNodeState) =>
              val x = if clusterNodeState.isActive then "active" else "maybe passive"
              logger.info(s"Selected $x ${clusterNodeState.nodeId} ${api.baseUri}")
              api
            }

  private case class ApiWithFiber[Api <: HttpClusterNodeApi](
    api: Api,
    fiber: Fiber[Checked[ClusterNodeState]])

  private case class ApiWithNodeState[Api <: HttpClusterNodeApi](
    api: Api,
    clusterNodeState: Checked[ClusterNodeState])

  private def fetchClusterNodeState(api: HttpClusterNodeApi): IO[Checked[ClusterNodeState]] =
    HttpClient
      .liftProblem(
        api.retryIfSessionLost()(
          api.clusterNodeState))
      .tapEval(checked => IO(
        logger.trace(s"${api.baseUri} => ${checked.fold(identity, identity)}")))

  private def logProblems[Api <: HttpClusterNodeApi](
    list: List[ApiWithNodeState[Api]],
    maybeActive: Option[(ClusterNodeApi, ClusterNodeState)],
    n: Int)
  : Unit =
    list.foreach:
      case ApiWithNodeState(api, Left(problem)) =>
        logger.warn(s"Cluster node ${api.baseUri} is not accessible: $problem")
      case _ =>
    // Different clusterStates only iff nodes are not coupled
    val clusterStates = list
      .collect:
        case ApiWithNodeState(api, Right(clusterNodeState)) =>
          s"${api.baseUri} => ${clusterNodeState.clusterState.getClass.simpleScalaName}"
      .mkString(" · ")
    if maybeActive.isEmpty then logger.warn(
      s"No cluster node (out of $n) seems to be active${clusterStates.nonEmpty ?? s": $clusterStates"}")
