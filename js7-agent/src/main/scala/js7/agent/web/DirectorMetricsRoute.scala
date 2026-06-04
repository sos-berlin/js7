package js7.agent.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.ConfigUtil
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}
import js7.common.metrics.{MetricFetcher, RemoteMetricsRoute}
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.data.agent.AgentRefState
import js7.data.subagent.SubagentItem
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.ContentType
import org.apache.pekko.http.scaladsl.server.{Directives, Route}

trait DirectorMetricsRoute extends AgentRouteProvider, RemoteMetricsRoute:

  protected def agentState: IO[Checked[AgentState]]
  protected def agentConfiguration: AgentConfiguration
  protected def actorSystem: ActorSystem

  private given IORuntime = ioRuntime
  private given ActorSystem = actorSystem

  protected final lazy val directorMetricsRoute: Route =
    wrapMetricsRoute: contentType =>
      import Directives.*
      parameter("deep" ? false):
        case false =>
          onlyThisServerMetricsRoute(contentType)
        case true =>
          deepMetricsRoute(contentType)

  private def deepMetricsRoute(contentType: ContentType): Route =
    ioRoute:
      agentState.flatMap: checkedAgentState =>
        completeMetricFetchers(
          contentType,
          localMetricFetcher ++:
            checkedAgentState.toOption.fold(Nil)(subagentMetricFetchers))

  private def subagentMetricFetchers(agentState: AgentState): List[MetricFetcher] =
    agentState.meta.directors.get(agentConfiguration.clusterConf.isBackup.toInt)
      .fold(Nil): ownSubagentId =>
        agentState.keyTo(AgentRefState).values.view.flatMap: agentRefState =>
          agentState.keyToItem(SubagentItem).values
        .filter:
          _.subagentId != ownSubagentId
        .map: subagentItem =>
          import subagentItem.{subagentId, uri}
          remoteMetricFetcher(
            serverId = subagentId.toJs7ServerId,
            admission = Admission(
              uri,
              agentConfiguration.config.optionAs[SecretString]:
                "js7.auth.subagents." + ConfigUtil.joinPath(subagentId.string)
              .map(UserAndPassword(ownSubagentId.toUserId.orThrow, _))),
            uriPath = "subagent",
            deep = false,
            label = subagentId.toString)
        .toList
