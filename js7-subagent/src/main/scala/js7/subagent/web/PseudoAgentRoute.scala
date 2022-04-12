package js7.subagent.web

import akka.http.scaladsl.model.StatusCodes.{BadRequest, NotFound, ServiceUnavailable}
import akka.http.scaladsl.server.Directives.{Segment, as, complete, entity, get, pathEnd, pathPrefix, post, withSizeLimit}
import akka.http.scaladsl.server.Route
import io.circe.JsonObject
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.problem.Problem
import js7.common.akkahttp.AkkaHttpServerUtils.{completeTask, pathSegment}
import js7.common.akkahttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.web.session.SessionRoute
import js7.core.web.EntitySizeLimitProvider
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.subagent.Problems.SubagentAlreadyDedicatedProblem
import js7.subagent.SubagentCommandExecutor
import monix.eval.Task

/** Looks like Agent Director web service to detect a client's request for an Director. */
private trait PseudoAgentRoute extends SessionRoute with EntitySizeLimitProvider
{
  protected val commandExecutor: SubagentCommandExecutor
  protected def restartAsDirector: Task[Unit]

  private implicit def implicitScheduler = scheduler

  protected final lazy val pseudoAgentRoute: Route =
    pathSegment("api")(
      pathPrefix(Segment) {
        case "session" => sessionRoute
        case "command" => pseudoAgentCommandRoute
        case "event" => pseudoAgentEventRoute
        case "clusterWatch" => pseudoAgentClusterWatchRoute
        case _ => complete(NotFound)
      })

  private lazy val pseudoAgentCommandRoute: Route =
    (pathEnd & post & withSizeLimit(entitySizeLimit))(
      authorizedUser(ValidUserPermission)(_ =>
        entity(as[JsonObject])(json =>
          checkSubagent(
            json(TypedJsonCodec.TypeFieldName).flatMap(_.asString) match {
              case Some("DedicateAgentDirector" | "CoupleController") =>
                completeWithRestartAsDirector

              case _ =>
                complete(AgentNotDedicatedProblem)
            }))))

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
        .as(ServiceUnavailable -> Problem(
          "Subagent becomes a fresh Agent Director - try again after a second")))
}
