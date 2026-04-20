package js7.controller.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.ConfigUtil
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.common.metrics.RemoteMetricsRoute
import js7.common.metrics.RemoteMetricsRoute.MetricFetcher
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.ControllerMetricsRoute.*
import js7.controller.web.common.ControllerRouteProvider
import js7.data.agent.{AgentPath, AgentRefState}
import js7.data.controller.ControllerState
import js7.data.node.{Js7ServerId, NodeNameToPassword}
import js7.data.subagent.SubagentItem
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.parameter
import org.apache.pekko.http.scaladsl.server.{Directives, Route}

trait ControllerMetricsRoute extends ControllerRouteProvider, RemoteMetricsRoute:

  protected def controllerState: IO[Checked[ControllerState]]
  protected val controllerConfiguration: ControllerConfiguration
  protected def actorSystem: ActorSystem

  private given IORuntime = ioRuntime
  private given ActorSystem = actorSystem

  private given NodeNameToPassword[ControllerState] =
    val result = Right(config.optionAs[SecretString]("js7.auth.cluster.password"))
    _ => result

  /** /metrics web service according to Prometheus.
    * <p>
    * Prometheus expects a web service path "/metrics".
    *
    * @see https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels
    * @see https://prometheus.io/docs/practices/naming/
    */
  protected final lazy val controllerMetricsRoute: Route =
    wrapMetricsRoute: contentType =>
      import Directives.*
      parameter("onlyThisServer" ? false):
        case true =>
          metricsRawRoute(contentType)
        case false =>
          ioRoute:
            controllerState.map:
              case Left(problem) =>
                logger.debug(s"Controller is not ready: $problem")
                Nil
              case Right(controllerState) =>
                clusterPeerMetricFetcher(controllerState).toSeq ++
                  agentMetricFetchers(controllerState)
            .flatMap:
              completeMetricFetchers(contentType, _)

  private def clusterPeerMetricFetcher(controllerState: ControllerState): Option[MetricFetcher] =
    controllerState.toPeerAndAdmission(controllerConfiguration.clusterConf.ownId).toOption
      .map: (_, peerAdmission, _) =>
        val serverId = controllerConfiguration.clusterConf.peerServerId
        fetchClusterPeerMetrics(serverId, peerAdmission)

  private def fetchClusterPeerMetrics(serverId: Js7ServerId, admission: Admission): MetricFetcher =
    remoteMetricFetcher(
      serverId,
      admission,
      uriPath = "controller",
      onlyThisServer = true,
      label = serverId.toString)

  private def agentMetricFetchers(controllerState: ControllerState): Seq[MetricFetcher] =
    val toSubagentItem = controllerState.keyToItem(SubagentItem)
    controllerState.keyTo(AgentRefState).values.toSeq.flatMap: agentRefState =>
      toSubagentItem.get(agentRefState.activeDirector).map: subagentItem =>
        fetchAgentMetrics(agentRefState.path, subagentItem)

  private def fetchAgentMetrics(agentPath: AgentPath, subagentItem: SubagentItem): MetricFetcher =
    remoteMetricFetcher(
      subagentItem.subagentId.toJs7ServerId,
      Admission(
        subagentItem.uri,
        controllerConfiguration.config.optionAs[SecretString]:
          "js7.auth.agents." + ConfigUtil.joinPath(agentPath.string)
        .map:
          UserAndPassword(controllerConfiguration.controllerId.unsafeUserId, _)),
      uriPath = "agent",
      onlyThisServer = false,
      label = agentPath.toString)


object ControllerMetricsRoute:
  private val logger = Logger[this.type]
