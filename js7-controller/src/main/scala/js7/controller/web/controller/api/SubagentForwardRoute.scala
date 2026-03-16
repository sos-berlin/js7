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
import js7.common.pekkohttp.StandardMarshallers.StatusCodeMarshaller
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.DirectorForwardRoute.forwardToDirector
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data.controller.ControllerState
import js7.data.subagent.{SubagentId, SubagentItem}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.StatusCodes.Forbidden
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{PathMatchers, Route}

trait SubagentForwardRoute extends ControllerRouteProvider:

  protected def actorSystem: ActorSystem
  protected def controllerConfiguration: ControllerConfiguration
  protected def controllerState: IO[Checked[ControllerState]]

  private given IORuntime = ioRuntime
  private given ActorSystem = actorSystem

  protected final lazy val subagentForwardRoute: Route =
    authorizedUser(ValidUserPermission): _ =>
      (get & pathPrefix(Segment)): subagentId =>
        checkedIoRoute:
          controllerState.map: checkedControllerState =>
            for
              subagentId <- SubagentId.checked(subagentId)
              controllerState <- checkedControllerState
              subagentItem <- controllerState.keyToItem(SubagentItem).checked(subagentId)
              subagentId = subagentItem.id
              agentPath = subagentItem.agentPath
              agentRefState <- controllerState.keyTo(AgentRefState).checked(agentPath)
              forwardingSubagentItem <-
                if agentRefState.item.directors.contains(subagentId) then
                  Right(subagentItem)
                else
                  controllerState.keyToItem(SubagentItem).checked(agentRefState.activeDirector)
            yield
              val userAndPassword =
                controllerConfiguration.config.optionAs[SecretString]:
                  "js7.auth.agents." + ConfigUtil.joinPath(agentPath.string)
                .map(UserAndPassword(controllerConfiguration.controllerId.unsafeUserId, _))
              rawPathPrefix(PathMatchers.Remaining): remainingUri =>
                if !remainingUri.startsWith("/log/") then
                  complete(Forbidden)
                else
                  forwardToDirector(
                    Admission(forwardingSubagentItem.uri, userAndPassword),
                    controllerConfiguration.httpsConfig,
                    forwardUri = remainingUri,
                    subagentId = (subagentId != forwardingSubagentItem.id) ? subagentId,
                    label = agentPath.toString)
