package js7.subagent.web

import akka.http.scaladsl.model.StatusCodes.{BadRequest, NotFound, ServiceUnavailable}
import akka.http.scaladsl.server.Directives.{Segment, as, complete, entity, get, pathEnd, pathEndOrSingleSlash, pathPrefix, post, withSizeLimit}
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
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.subagent.Problems.SubagentAlreadyDedicatedProblem
import js7.data.subagent.SubagentCommand
import js7.subagent.SubagentCommandExecutor
import js7.subagent.web.PseudoAgentRoute.*
import monix.eval.Task
import monix.execution.Scheduler

/** Looks like Agent Director web service to detect a client's request for an Director. */
private trait PseudoAgentRoute extends SessionRoute with EntitySizeLimitProvider
{
  protected def executeCommand(command: Numbered[SubagentCommand])
  : Task[Checked[SubagentCommand.Response]]

  protected val commandExecutor: SubagentCommandExecutor
  protected def restartAsDirector: Task[Unit]
  protected def overviewRoute: Route

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val pseudoAgentRoute: Route =
    pathSegment("api")(
      pathEndOrSingleSlash(overviewRoute) ~
        pathPrefix(Segment) {
          case "session" => sessionRoute
          case "command" => agentCommandRoute
          case "event" => pseudoAgentEventRoute
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
                completeWithRestartAsDirector)

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

  private lazy val pseudoAgentClusterWatchRoute: Route =
    (pathEnd & (post | get))(
      authorizedUser(ValidUserPermission)(_ =>
        checkSubagent(
          // Simply touching this web service is enough to restart as an Agent Director
          completeWithRestartAsDirector)))

  private def checkSubagent(route: Route): Route =
    if (commandExecutor.checkedDedicated.isRight)
      complete(BadRequest -> SubagentAlreadyDedicatedProblem)
    else
      route

  private def completeWithRestartAsDirector =
    completeTask(
      restartAsDirector
        .delayExecution(200.ms) // Delay in background to allow to respond properly (for test)
        .onErrorHandle(t => Task {
          logger.error(s"restartAsDirector => ${t.toStringWithCauses}")
        })
        .start
        .as(ServiceUnavailable -> Problem(
          "Subagent becomes a fresh Agent Director - try again after a second")))
}

object PseudoAgentRoute
{
  private val logger = Logger[this.type]
}
