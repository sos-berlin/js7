package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.data.MasterOverview
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait ApiRootRoute {

  protected def executeCommand(command: MasterCommand): Future[MasterCommand.Response]
  protected def orderCountFuture: Future[Int]
  protected implicit def executionContext: ExecutionContext

  final val apiRootRoute: Route =
    pathEnd {
      get {
        complete(
          for (orderCount ← orderCountFuture) yield
            MasterOverview(
              version = BuildInfo.buildVersion,
              buildId = BuildInfo.buildId,
              startedAt = RunningMaster.StartedAt.toTimestamp,
              orderCount = orderCount,
              system = systemInformation(),
              java = javaInformation))
      } ~
      post {
        entity(as[MasterCommand]) { command ⇒
          complete {
            executeCommand(command)
          }
        }
      }
    }
}
