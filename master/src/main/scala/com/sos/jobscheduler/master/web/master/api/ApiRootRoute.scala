package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterOverview
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute extends MasterRouteProvider {

  protected def masterId: MasterId
  protected def orderCount: Task[Int]

  private implicit def implicitScheduler = scheduler

  final val apiRootRoute: Route =
    pathEnd {
      get {
        complete(overview)
      }
    }

  private def overview: Task[MasterOverview] =
    for (orderCount ← orderCount) yield
      MasterOverview(
        id = masterId,
        version = BuildInfo.buildVersion,
        buildId = BuildInfo.buildId,
        startedAt = RunningMaster.StartedAt,
        orderCount = orderCount,
        system = systemInformation(),
        java = javaInformation)
}
