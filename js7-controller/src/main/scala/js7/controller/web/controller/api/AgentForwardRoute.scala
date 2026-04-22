package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.ConfigUtil
import js7.base.auth.{Admission, UserAndPassword, ValidUserPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.pekkohttp.StandardDirectives.checkedIoRoute
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.DirectorForwardRoute.forwardToDirector
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data.controller.ControllerState
import js7.data.subagent.SubagentItem
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.StatusCodes.Forbidden
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{PathMatchers, Route}

trait AgentForwardRoute extends ControllerRouteProvider:

  protected implicit def actorSystem: ActorSystem
  protected def controllerConfiguration: ControllerConfiguration
  protected def controllerState: IO[Checked[ControllerState]]

  private given IORuntime = ioRuntime

  protected final lazy val agentForwardRoute: Route =
    authorized(ValidUserPermission):
      get:
        pathPrefix(Segment): agentPath =>
          rawPathPrefix(PathMatchers.Remaining): remainingUri =>
            if remainingUri.nonEmpty then
              complete(Forbidden)
            else
              checkedIoRoute:
                controllerState.map: checkedControllerState =>
                  for
                    controllerState <- checkedControllerState
                    agentPath <- AgentPath.checked(agentPath)
                    agentRefState <- controllerState.keyTo(AgentRefState).checked(agentPath)
                    activeSubagentItem <-
                      controllerState.keyToItem(SubagentItem).checked(agentRefState.activeDirector)
                  yield
                    forwardToDirector(
                      Admission(
                        activeSubagentItem.uri,
                        controllerConfiguration.config.optionAs[SecretString]:
                          "js7.auth.agents." + ConfigUtil.joinPath(agentPath.string)
                        .map:
                          UserAndPassword(controllerConfiguration.controllerId.unsafeUserId, _)),
                      controllerConfiguration.httpsConfig,
                      forwardUri = remainingUri,
                      label = agentPath.toString)
