package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.BuildInfo
import js7.base.problem.Checked
import js7.base.system.SystemInformations.systemInformation
import js7.base.system.startup.StartUp
import js7.base.time.ScalaTime.*
import js7.common.pekkohttp.CirceJsonSupport.*
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.system.JavaInformations.javaInformation
import js7.controller.web.common.ControllerRouteProvider
import js7.data.controller.{ControllerId, ControllerOverview, ControllerState}
import org.apache.pekko.http.scaladsl.server.Directives.{get, pathEndOrSingleSlash}
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute extends ControllerRouteProvider:

  protected def controllerId: ControllerId
  protected def controllerState: IO[Checked[ControllerState]]
  protected def totalRunningSince: Deadline

  private given IORuntime = ioRuntime

  final val apiRootRoute: Route =
    pathEndOrSingleSlash:
      get:
        completeIO:
          overview

  private def overview: IO[ControllerOverview] =
    for checkedControllerState <- controllerState yield
      js7.data.controller.ControllerOverview(
        id = controllerId,
        version = BuildInfo.prettyVersion,
        buildId = BuildInfo.buildId,
        initiallyStartedAt = checkedControllerState.toOption
          .map(_.controllerMetaState.initiallyStartedAt),
        startedAt = StartUp.startedAt,
        totalRunningTime = totalRunningSince.elapsed.roundUpToNext(1.ms),
        orderCount = checkedControllerState.toOption.map(_.idToOrder.size),
        system = systemInformation(),
        java = javaInformation())
