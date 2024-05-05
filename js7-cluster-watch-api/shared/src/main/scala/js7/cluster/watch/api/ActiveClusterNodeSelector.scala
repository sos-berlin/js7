package js7.cluster.watch.api

import cats.effect.std.Supervisor
import cats.effect.{FiberIO, IO, Resource, ResourceIO}
import cats.syntax.flatMap.*
import fs2.Stream
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.utils.CatsUtils.{Nel, continueWithLast}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.data.cluster.{ClusterNodeApi, ClusterNodeState}
import scala.concurrent.duration.FiniteDuration

object ActiveClusterNodeSelector:

  private val logger = Logger[this.type]

  /** Selects the API for the active node, waiting until one is active.
   * The returned EventApi is logged-in. */
  final def selectActiveNodeApi[Api <: HttpClusterNodeApi](
    apisResource: ResourceIO[Nel[Api]],
    failureDelays: Nel[FiniteDuration],
    onCouplingError: Api => Throwable => IO[Unit] =
      (_: Api) => (t: Throwable) => SessionApi.onErrorTryAgain(toString, t).void)
    (using Tag[Api])
  : ResourceIO[Api] =
    logger.traceResource:
      apisResource.flatMap: apis =>
        Resource.eval:
          selectActiveNodeApiOnly[Api](
            apis,
            api => throwable => onCouplingError(api)(throwable),
            failureDelays)

  private def selectActiveNodeApiOnly[Api <: HttpClusterNodeApi](
    apis: Nel[Api],
    onCouplingError: Api => Throwable => IO[Unit],
    failureDelays: Nel[FiniteDuration])
  : IO[Api] =
    logger.traceIOWithResult:
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
            ().tailRecM: _ =>
              Supervisor[IO].use: supervisor =>
                apis.traverse: api =>
                  supervisor.supervise:
                    fetchClusterNodeState(api)
                      .catchIntoChecked /*don't let the whole operation fail*/
                  .map(ApiWithFiber(api, _))
                .flatMap: (apisWithClusterNodeStateFibers: Nel[ApiWithFiber[Api]]) =>
                  Stream.iterable(apisWithClusterNodeStateFibers.toList)
                    .covary[IO]
                    .map: o =>
                      Stream.eval(o.fiber.joinStd).map(ApiWithNodeState(o.api, _))
                    // Query nodes in parallel and continue with first response first
                    .parJoinUnbounded
                    .takeThrough(o => !o.clusterNodeState.exists(_.isActive))
                    .compile
                    .toList
                    .flatMap: list =>
                      val maybeActive = list.lastOption.collect:
                        case ApiWithNodeState(api, Right(nodeState)) if nodeState.isActive =>
                          api -> nodeState
                      logProblems(list, maybeActive, n = apis.length)
                      maybeActive match
                        case None => IO.sleep(failureDelayIterator.next()).as(Left(()))
                        case Some(x) => IO.right(x)
            .onErrorRestartLoop(()): (throwable, _, tryAgain) =>
              logger.warn(throwable.toStringWithCauses)
              if throwable.getStackTrace.nonEmpty then logger.debug(s"💥 $throwable", throwable)
              tryAgain(()).delayBy(failureDelayIterator.next())
            .map: (api, clusterNodeState) =>
              val x = if clusterNodeState.isActive then "active" else "maybe passive"
              logger.info(s"Selected $x ${clusterNodeState.nodeId} ${api.baseUri}")
              api

  private case class ApiWithFiber[Api <: HttpClusterNodeApi](
    api: Api,
    fiber: FiberIO[Checked[ClusterNodeState]])

  private case class ApiWithNodeState[Api <: HttpClusterNodeApi](
    api: Api,
    clusterNodeState: Checked[ClusterNodeState])

  private def fetchClusterNodeState(api: HttpClusterNodeApi): IO[Checked[ClusterNodeState]] =
    HttpClient
      .liftProblem(
        api.retryIfSessionLost()(
          api.clusterNodeState))
      .flatTap(checked => IO(
        logger.trace(s"${api.baseUri} => ${checked.merge}")))

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
