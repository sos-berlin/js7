package js7.cluster.watch.api

import cats.effect.std.Supervisor
import cats.effect.{FiberIO, IO, Resource, ResourceIO}
import cats.syntax.flatMap.*
import fs2.Stream
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.generic.Completed
import js7.base.log.{LogLevel, Logger}
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{DelayConf, Delayer}
import js7.base.web.HttpClient
import js7.data.cluster.{ClusterNodeApi, ClusterNodeState}
import scala.math.Ordered.orderingToOrdered

final class ActiveClusterNodeSelector[Api <: HttpClusterNodeApi](
  apisResource: ResourceIO[Nel[Api]],
  delayConf: DelayConf,
  clusterName: String = "",
  onCouplingError: Api => Throwable => IO[Unit] =
    (_: Api) => (t: Throwable) => SessionApi.onErrorTryAgain(toString, t).void)
  (using Tag[Api]):

  private val logger = Logger.withPrefix[this.type](clusterName)

  private def selectActiveNodeApi: ResourceIO[Api] =
    logger.traceResource:
      apisResource.flatMap: apis =>
        Resource.eval:
          selectActiveNodeApiOnly(
            apis,
            api => throwable => onCouplingError(api)(throwable))

  private def selectActiveNodeApiOnly(apis: Nel[Api], onCouplingError: Api => Throwable => IO[Unit]): IO[Api] =
    logger.traceIOWithResult:
      apis match
        case Nel(api, Nil) =>
          api
            .loginUntilReachable(
              onError = t => onCouplingError(api)(t).as(true),
              onlyIfNotLoggedIn = true)
            .map((_: Completed) => api)

        case _ =>
          delayConf.runIO: delayer =>
            ().tailRecM: _ =>
              Supervisor[IO].use: supervisor =>
                apis.traverse: api =>
                  supervisor.supervise:
                    fetchClusterNodeState(api)
                      .catchIntoChecked /*don't let the whole operation fail*/
                  .map(ApiWithFiber(api, _))
                .flatMap: (apisWithClusterNodeStateFibers: Nel[ApiWithFiber]) =>
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
                      logNonActive(delayer, list, maybeActive, n = apis.length)
                      maybeActive match
                        case None => delayer.sleep.as(Left(()))
                        case Some(x) => IO.right(x)
                .handleErrorWith: throwable =>
                  logger.log(delayer.logLevel, s"${delayer.symbol} ${throwable.toStringWithCauses}")
                  if throwable.getStackTrace.nonEmpty then logger.debug(s"💥 $throwable", throwable)
                  delayer.sleep.as(Left(()))
            .map: (api, clusterNodeState) =>
              val sym = (delayer.logLevel >= LogLevel.Info) ?? s"🟢 "
              val x = if clusterNodeState.isActive then "active" else "maybe passive"
              logger.info:
                s"${sym}Selected $x ${clusterNodeState.nodeId} ${api.baseUri}"
              api

  private def fetchClusterNodeState(api: Api): IO[Checked[ClusterNodeState]] =
    HttpClient.liftProblem:
      api.retryIfSessionLost:
        api.clusterNodeState
    .flatTap: checked =>
      IO(logger.trace(s"${api.baseUri} => ${checked.merge}"))

  private def logNonActive(
    delayer: Delayer[IO],
    list: List[ApiWithNodeState],
    maybeActive: Option[(ClusterNodeApi, ClusterNodeState)],
    n: Int)
  : Unit =
    list.foreach:
      case ApiWithNodeState(api, Left(problem)) =>
        logger.warn(s"Cluster node ${api.baseUri} is not accessible: $problem")
      case _ =>

    if maybeActive.isEmpty then
      // Different clusterStates only iff nodes are not coupled
      val clusterStates = list.collect:
        case ApiWithNodeState(api, Right(clusterNodeState)) =>
          s"${api.baseUri} => ${clusterNodeState.clusterState.getClass.simpleScalaName}"

      logger.log(delayer.logLevel, s"${delayer.symbol
        } None of the $n cluster nodes seems to be active${
        clusterStates.nonEmpty ?? s": ${clusterStates.mkString(" · ")}"}")


  private case class ApiWithFiber(
    api: Api,
    fiber: FiberIO[Checked[ClusterNodeState]])

  private case class ApiWithNodeState(
    api: Api,
    clusterNodeState: Checked[ClusterNodeState])


object ActiveClusterNodeSelector:

  /** Selects the API for the active node, waiting until one is active. +/
    * The returned EventApi is logged-in. */
  def selectActiveNodeApi[Api <: HttpClusterNodeApi](
    apisResource: ResourceIO[Nel[Api]],
    delayConf: DelayConf,
    onCouplingError: Api => Throwable => IO[Unit] =
      (_: Api) => (t: Throwable) => SessionApi.onErrorTryAgain(toString, t).void,
    clusterName: String = "")
    (using Tag[Api])
  : ResourceIO[Api] =
    ActiveClusterNodeSelector(apisResource, delayConf, clusterName, onCouplingError)
      .selectActiveNodeApi
