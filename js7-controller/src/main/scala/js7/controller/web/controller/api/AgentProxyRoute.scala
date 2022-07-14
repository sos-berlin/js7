package js7.controller.web.controller.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, HttpRequest, HttpResponse, headers, Uri => AkkaUri}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.agent.client.AgentClient
import js7.base.auth.{SessionToken, ValidUserPermission}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.http.AkkaHttpUtils.RichAkkaUri
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.AgentProxyRoute.*
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.controller.ControllerState
import monix.eval.Task
import monix.execution.Scheduler
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
            uri -> uri.asAkka.copy(
              path = AkkaUri.Path((uri.asAkka.path ?/ "agent" / "api").toString),
              rawQueryString = request.uri.rawQueryString))
          .toRight(Problem.pure("AgentRef has no URI"))))
      .flatMap {
        case Left(problem) =>
          Task.pure(HttpResponse(
            problem.httpStatusCode,
            // Encoding: application/json, or use standard marshaller ???
            entity = HttpEntity(problem.toString)))

        case Right((uri, akkaUri)) =>
          forwardTo(agentRef, uri, akkaUri, request.headers)
      }
  }

  private def forwardTo(agentRef: AgentRef, uri: Uri, akkaUri: AkkaUri, headers: Seq[HttpHeader])
  : Task[HttpResponse] = {
    val agentClient = AgentClient(  // TODO Reuse AgentClient of AgentDriver
      uri,
      userAndPassword = None,
      label = agentRef.path.toString,
      controllerConfiguration.httpsConfig)
    implicit val sessionToken: Task[Option[SessionToken]] = Task.pure(None)
    agentClient
      .sendReceive(HttpRequest(GET,  akkaUri, headers = headers.filter(h => isForwardableHeaderClass(h.getClass))))
      .map(response => response.withHeaders(response.headers.filterNot(h => IsIgnoredAgentHeader(h.getClass))))
      .guarantee(Task(agentClient.close()))
  }
}

object AgentProxyRoute {
  private val isForwardableHeaderClass = Set[Class[_ <: HttpHeader]](
    classOf[Accept],
    classOf[`Cache-Control`])

  private val IsIgnoredAgentHeader: Set[Class[_ <: HttpHeader]] = Set(
    //classOf[headers.Server],
    //classOf[headers.Date],
    classOf[headers.`Cache-Control`])
    //classOf[headers.`Content-Type`],
    //classOf[headers.`Content-Length`])
}
