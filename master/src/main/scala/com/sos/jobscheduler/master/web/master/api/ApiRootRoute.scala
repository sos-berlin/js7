package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.command.CommandMeta
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute extends MasterRouteProvider {

  protected def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[MasterCommand.Response]]
  protected def orderCount: Task[Int]
  protected implicit def scheduler: Scheduler

  final val apiRootRoute: Route =
    pathEnd {
      get {
        authorizedUser() { _ ⇒
          complete(overview)
        }
      } ~
      post {
        authorizedUser() { user ⇒
          entity(as[MasterCommand]) { command ⇒
            complete {
              executeCommand(command, CommandMeta(user))
            }
          }
        }
      }
    }

  private def overview: Task[MasterOverview] =
    for (orderCount ← orderCount) yield
      MasterOverview(
        version = BuildInfo.buildVersion,
        buildId = BuildInfo.buildId,
        startedAt = RunningMaster.StartedAt,
        orderCount = orderCount,
        system = systemInformation(),
        java = javaInformation)
}
