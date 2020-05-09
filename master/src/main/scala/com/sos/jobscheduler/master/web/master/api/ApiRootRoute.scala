package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.BuildInfo
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.{MasterOverview, MasterState}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
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
