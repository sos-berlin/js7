package js7.subagent.web

import akka.http.scaladsl.model.StatusCodes.{BadRequest, NotFound, ServiceUnavailable}
import akka.http.scaladsl.server.Directives.{Segment, as, complete, entity, get, parameter, path, pathEnd, pathEndOrSingleSlash, pathPrefix, post, withSizeLimit}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.RouteConcatenation.*
import cats.syntax.traverse.*
import io.circe.{Json, JsonObject}
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.RichCirceEither
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.akkahttp.AkkaHttpServerUtils.{completeTask, pathSegment}
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.SessionRoute
import js7.core.web.EntitySizeLimitProvider
import js7.data.agent.AgentClusterConf
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.cluster.{ClusterNodeState, ClusterState}
import js7.data.subagent.Problems.SubagentAlreadyDedicatedProblem
import js7.data.subagent.SubagentCommand
import js7.subagent.Subagent
import js7.subagent.web.PseudoDirectorRoute.*
import monix.eval.Task
import monix.execution.Scheduler

/** Looks like Agent Director web service to detect a client's request for an Director. */
private trait PseudoDirectorRoute extends SessionRoute with EntitySizeLimitProvider
{
  protected def executeCommand(command: Numbered[SubagentCommand])
  : Task[Checked[SubagentCommand.Response]]

  protected val subagent: Subagent
  protected def convertToDirector: Task[Unit]
  protected def overviewRoute: Route

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val pseudoAgentRoute: Route =
    pathSegment("api")(
      pathEndOrSingleSlash(overviewRoute) ~
        pathPrefix(Segment) {
          case "session" => sessionRoute
          case "command" => agentCommandRoute
          case "event" => pseudoAgentEventRoute
          case "cluster" => pseudoClusterRoute
          case "clusterWatch" => pseudoAgentClusterWatchRoute
          case _ => complete(NotFound)
        })

  private lazy val agentCommandRoute: Route =
    (pathEnd & post & withSizeLimit(entitySizeLimit))(
      authorizedUser(ValidUserPermission)(_ =>
        entity(as[JsonObject])(json =>
          json(TypedJsonCodec.TypeFieldName).flatMap(_.asString) match {
            case Some("DedicateAgentDirector" | "CoupleController") =>
              checkSubagent(
                completeWithConvertToDirector)

            case typeName =>
              if (typeName contains SubagentCommand.jsonCodec.typeName[SubagentCommand.ShutDown])
                completeTask(
                  Json.fromJsonObject(json)
                    .as[SubagentCommand]
                    .toChecked
                    .traverse(cmd => executeCommand(Numbered(0, cmd))))
              else
                checkSubagent(
                  complete(AgentNotDedicatedProblem))
          })))

  private lazy val pseudoAgentEventRoute: Route =
    (pathEnd & get)(
      authorizedUser(ValidUserPermission)(_ =>
        checkSubagent(
          complete(AgentNotDedicatedProblem))))

  private lazy val pseudoClusterRoute: Route =
    pathEnd(get(
      parameter("return") {
        case "ClusterNodeState" =>
          // FIXME Sollte ActiveClusterNodeSelector das selbst erkennen?
          //  Dann können wir auf diesen Pseudo-Webservice verzichten und auch auf is-backup.
          val isBackup = config.getBoolean("js7.journal.cluster.node.is-backup")
          val ownClusterNodeId =
            if (isBackup) AgentClusterConf.backupNodeId else AgentClusterConf.primaryNodeId
          complete(
            ClusterNodeState(ownClusterNodeId, isBackup = isBackup, ClusterState.Empty))

        case _ =>
          complete(NotFound)
      })
    ) ~
      path("command")(post(
        authorizedUser(ValidUserPermission)(_ =>
          checkSubagent(
            // Simply touching this web service is enough to restart as an Agent Director
            // Limit to ClusterStartBackupNode ???
            completeWithConvertToDirector))))

  private lazy val pseudoAgentClusterWatchRoute: Route =
    (pathEnd & (post | get))(
      authorizedUser(ValidUserPermission)(_ =>
        checkSubagent(
          // Simply touching this web service is enough to restart as an Agent Director
          completeWithConvertToDirector)))

  private def checkSubagent(route: Route): Route =
    if (subagent.checkedDedicated.isRight)
      complete(BadRequest -> SubagentAlreadyDedicatedProblem)
    else
      route

  private def completeWithConvertToDirector =
    completeTask(
      // Delay in background to allow to respond properly before WebServer shuts down (for testing)
      Task.sleep(200.ms)
        .*>(convertToDirector)
        .onErrorHandle(t => logger.error(s"convertToDirector => ${t.toStringWithCauses}"))
        .startAndForget
        .as(ServiceUnavailable -> Problem(
          "Subagent is converting to an Agent Director - try again in a second")))
}

object PseudoDirectorRoute
{
  private val logger = Logger[this.type]
}
