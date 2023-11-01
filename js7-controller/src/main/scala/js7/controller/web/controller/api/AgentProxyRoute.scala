package js7.controller.web.controller.api

import js7.agent.client.AgentClient
import js7.base.auth.{SessionToken, ValidUserPermission}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.http.PekkoHttpUtils.RichPekkoUri
import js7.common.pekkohttp.PekkoHttpServerUtils.completeTask
import js7.common.pekkohttp.StandardMarshallers.*
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.AgentProxyRoute.*
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.controller.ControllerState
import monix.eval.Task
import monix.execution.Scheduler
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.HttpMethods.GET
import org.apache.pekko.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import org.apache.pekko.http.scaladsl.model.{HttpEntity, HttpHeader, HttpRequest, HttpResponse, headers, Uri as PekkoUri}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import scala.collection.MapView

/**
  * @author Joacim Zschimmer
  */
trait AgentProxyRoute extends ControllerRouteProvider
{
  protected implicit def actorSystem: ActorSystem
  protected def pathToAgentRefState: Task[Checked[MapView[AgentPath, AgentRefState]]]
  protected def controllerConfiguration: ControllerConfiguration
  protected def controllerState: Task[Checked[ControllerState]]

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentProxyRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        path(Segment) { pathString =>
          extractRequest { request =>
            completeTask(
              Task.pure(AgentPath.checked(pathString))
                .flatMapT(name => pathToAgentRefState.map(_.flatMap(_.checked(name))))
                .flatMapT(agentRefState =>
                  forward(agentRefState.agentRef, request)
                    .map(Right.apply)))
          }
        }
      }
    }

  private def forward(agentRef: AgentRef, request: HttpRequest): Task[HttpResponse] = {
    controllerState
      .map(_.flatMap(s =>
        s.agentToUri(agentRef.path)
          .map(uri =>
            uri -> uri.asPekko.copy(
              path = PekkoUri.Path((uri.asPekko.path ?/ "agent" / "api").toString),
              rawQueryString = request.uri.rawQueryString))
          .toRight(Problem.pure("AgentRef has no URI"))))
      .flatMap {
        case Left(problem) =>
          Task.pure(HttpResponse(
            problem.httpStatusCode,
            // Encoding: application/json, or use standard marshaller ???
            entity = HttpEntity(problem.toString)))

        case Right((uri, pekkoUri)) =>
          forwardTo(agentRef, uri, pekkoUri, request.headers)
      }
  }

  private def forwardTo(agentRef: AgentRef, uri: Uri, pekkoUri: PekkoUri, headers: Seq[HttpHeader])
  : Task[HttpResponse] = {
    val agentClient = AgentClient(  // TODO Reuse AgentClient of AgentDriver
      uri,
      userAndPassword = None,
      label = agentRef.path.toString,
      controllerConfiguration.httpsConfig)
    implicit val sessionToken: Task[Option[SessionToken]] = Task.pure(None)
    agentClient
      .sendReceive(HttpRequest(GET,  pekkoUri, headers = headers.filter(h => isForwardableHeaderClass(h.getClass))))
      .map(response => response.withHeaders(response.headers.filterNot(h => IsIgnoredAgentHeader(h.getClass))))
      .guarantee(Task(agentClient.close()))
  }
}

object AgentProxyRoute {
  private val isForwardableHeaderClass = Set[Class[? <: HttpHeader]](
    classOf[Accept],
    classOf[`Cache-Control`])

  private val IsIgnoredAgentHeader: Set[Class[? <: HttpHeader]] = Set(
    //classOf[headers.Server],
    //classOf[headers.Date],
    classOf[headers.`Cache-Control`])
    //classOf[headers.`Content-Type`],
    //classOf[headers.`Content-Length`])
}
