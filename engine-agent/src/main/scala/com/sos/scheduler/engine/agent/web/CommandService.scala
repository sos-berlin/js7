package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.data.commandresponses.Response
import com.sos.scheduler.engine.agent.data.commands.Command
import com.sos.scheduler.engine.agent.web.CommandService._
import com.sos.scheduler.engine.agent.web.common.ServiceStandards
import com.sos.scheduler.engine.common.soslicense.{LicenseKeyBunch, LicenseKeyChecker}
import com.sos.scheduler.engine.common.sprayutils.SprayJsonOrYamlSupport._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.routing.Directives._

/**
 * @author Joacim Zschimmer
 */
trait CommandService extends ServiceStandards {

  protected def executeCommand(command: Command, licenseKey: Option[LicenseKeyChecker]): Future[Response]

  addApiRoute {
    (path("command") & post) {
      optionalHeaderValueByName(LicenseKeyHeaderName) { licenseKeys ⇒
        entity(as[Command]) { command ⇒
          val future = executeCommand(command, licenseKeys map LicenseKeyBunch.apply)
          onSuccess(future) { response: Response ⇒ complete(response) }
        }
      }
    }
  }
}

object CommandService {
  private val LicenseKeyHeaderName = "X-JobScheduler-LicenseKey"     // Duplicate in AgentUris
}
