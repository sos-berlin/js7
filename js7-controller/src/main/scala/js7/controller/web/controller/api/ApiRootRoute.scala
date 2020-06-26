package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.BuildInfo
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.controller.data.{ControllerOverview, ControllerState}
import js7.controller.web.common.ControllerRouteProvider
import js7.data.controller.ControllerId
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute extends ControllerRouteProvider
{
  protected def controllerId: ControllerId
  protected def controllerState: Task[Checked[ControllerState]]
  protected def totalRunningSince: Deadline

  private implicit def implicitScheduler: Scheduler = scheduler

  final val apiRootRoute: Route =
    pathEnd {
      get {
        completeTask(
          overview)
      }
    }

  private def overview: Task[ControllerOverview] =
    for (checkedControllerState <- controllerState) yield
      ControllerOverview(
        id = controllerId,
        version = BuildInfo.prettyVersion,
        buildId = BuildInfo.buildId,
        startedAt = checkedControllerState.toOption.map(_.controllerMetaState.startedAt),
        totalRunningTime = totalRunningSince.elapsed roundUpToNext 1.ms,
        orderCount = checkedControllerState.toOption.map(_.idToOrder.size),
        system = systemInformation(),
        java = javaInformation)
}
