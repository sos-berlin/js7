package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.MasterState
import com.sos.jobscheduler.master.data.MasterOverview
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute extends MasterRouteProvider
{
  protected def masterId: MasterId
  protected def masterState: Task[MasterState]
  protected def totalRunningTime: Task[FiniteDuration]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val apiRootRoute: Route =
    pathEnd {
      get {
        completeTask(
          overview)
      }
    }

  private def overview: Task[MasterOverview] =
    Task.parMap2(masterState, totalRunningTime)((masterState, totalRunningTime) =>
      MasterOverview(
        id = masterId,
        version = BuildInfo.prettyVersion,
        buildId = BuildInfo.buildId,
        startedAt = masterState.masterMetaState.startedAt,
        orderCount = masterState.orders.size,
        system = systemInformation(),
        java = javaInformation,
        totalRunningTime = totalRunningTime))
}
