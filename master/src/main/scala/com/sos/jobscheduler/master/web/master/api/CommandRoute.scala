package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.master.command.CommandMeta
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait CommandRoute extends MasterRouteProvider {

  protected def executeCommand(command: MasterCommand, meta: CommandMeta): Task[Checked[MasterCommand.Response]]
  protected implicit def scheduler: Scheduler

  final val commandRoute: Route =
    pathEnd {
      post {
        authorizedUser(ValidUserPermission) { user ⇒
          entity(as[MasterCommand]) { command ⇒
            complete {
              executeCommand(command, CommandMeta(user))
            }
          }
        }
      }
    }
}
