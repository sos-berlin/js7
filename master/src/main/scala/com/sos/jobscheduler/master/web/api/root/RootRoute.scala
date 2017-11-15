package com.sos.jobscheduler.master.web.api.root

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.SprayJsonOrYamlSupport._
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.data.system.JavaInformation
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.command.MasterCommand
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait RootRoute {

  protected def executeCommand(command: MasterCommand): Future[MasterCommand.Response]
  protected def orderCountFuture: Future[Int]
  protected implicit def executionContext: ExecutionContext

  final val rootRoute: Route =
    pathEnd {
      get {
        complete(
          for (orderCount ← orderCountFuture) yield
            MasterOverview(
              version = RunningMaster.VersionString,
              startedAt = RunningMaster.StartedAt,
              orderCount = orderCount,
              system = systemInformation(),
              java = JavaInformation()))
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
