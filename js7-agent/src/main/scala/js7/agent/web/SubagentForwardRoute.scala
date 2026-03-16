package js7.agent.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.ConfigUtil
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.{Admission, SessionToken, UserAndPassword, ValidUserPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.PekkoHttpUtils.RichPekkoUri
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.StandardDirectives.checkedIoRoute
import js7.common.pekkohttp.StandardMarshallers.StatusCodeMarshaller
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.director.HttpSubagentApi
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.StatusCodes.Forbidden
import org.apache.pekko.http.scaladsl.model.Uri as PekkoUri
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{PathMatchers, Route}

trait SubagentForwardRoute extends AgentRouteProvider:

  protected def agentState: IO[Checked[AgentState]]
  protected def actorSystem: ActorSystem
  protected def agentConfiguration: AgentConfiguration

  private given ActorSystem = actorSystem
  private given IORuntime = ioRuntime

  protected final lazy val subagentForwardRoute: Route =
    authorizedUser(ValidUserPermission): _ =>
      (get & pathPrefix(Segment)): subagentId =>
        checkedIoRoute:
          agentState.map: checkedAgentState =>
            for
              subagentId <- SubagentId.checked(subagentId)
              agentState <- checkedAgentState
              subagentItem <- agentState.keyToItem(SubagentItem).checked(subagentId)
            yield
              val ownSubagentId =
                if agentConfiguration.clusterConf.isBackup then
                  agentState.meta.directors(1)
                else
                  agentState.meta.directors.head
              val subagentId = subagentItem.id
              val admission = Admission(
                subagentItem.uri,
                agentConfiguration.config.optionAs[SecretString]:
                  "js7.auth.subagents." + ConfigUtil.joinPath(subagentId.string)
                .map(UserAndPassword(ownSubagentId.toUserId.orThrow, _)))
              rawPathPrefix(PathMatchers.Remaining): remainingUri =>
                if !remainingUri.startsWith("/log/") then
                  complete(Forbidden)
                else
                  forward(admission, subagentId, remainingUri)

  private def forward(admission: Admission, subagentId: SubagentId, remainingUri: String) =
    extractRequest: request =>
      completeIO:
        HttpSubagentApi.resource(
          admission, agentConfiguration.httpsConfig, name = subagentId.string
        ).use: subagentApi =>
          given IO[Option[SessionToken]] = IO(subagentApi.sessionToken)
          val pekkoUri = admission.uri.asPekko
          subagentApi.login() *>
            subagentApi.forward(
              request,
              pekkoUri.copy(
                path = pekkoUri.path ?/ "subagent" / "api" ++ PekkoUri.Path(remainingUri),
                rawQueryString = request.uri.rawQueryString), dontLog = true)
