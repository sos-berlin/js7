package js7.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.BuildInfo
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.data.master.MasterId
import js7.master.data.{MasterOverview, MasterState}
import js7.master.web.common.MasterRouteProvider
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute extends MasterRouteProvider
{
  protected def masterId: MasterId
  protected def masterState: Task[Checked[MasterState]]
  protected def totalRunningSince: Deadline

  private implicit def implicitScheduler: Scheduler = scheduler

  final val apiRootRoute: Route =
    pathEnd {
      get {
        completeTask(
          overview)
      }
    }

  private def overview: Task[MasterOverview] =
    for (checkedMasterState <- masterState) yield
      MasterOverview(
        id = masterId,
        version = BuildInfo.prettyVersion,
        buildId = BuildInfo.buildId,
        startedAt = checkedMasterState.toOption.map(_.masterMetaState.startedAt),
        totalRunningTime = totalRunningSince.elapsed roundUpToNext 1.ms,
        orderCount = checkedMasterState.toOption.map(_.idToOrder.size),
        system = systemInformation(),
        java = javaInformation)
}
