package js7.controller.web.controller.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, headers, Uri => AkkaUri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.client.AgentClient
import js7.base.auth.{SessionToken, ValidUserPermission}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.StandardMarshallers._
import js7.common.http.AkkaHttpUtils.RichAkkaUri
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.AgentProxyRoute._
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentProxyRoute extends ControllerRouteProvider
{
  protected implicit def actorSystem: ActorSystem
  protected def pathToAgentRefState: Task[Checked[Map[AgentPath, AgentRefState]]]
  protected def controllerConfiguration: ControllerConfiguration

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
    val agentUri = agentRef.uri
    val uri = agentUri
      .asAkka.copy(
        path = AkkaUri.Path((agentUri.asAkka.path ?/ "agent" / "api").toString),
        rawQueryString = request.uri.rawQueryString)
    forwardTo(agentRef, uri, request.headers)
  }

  private def forwardTo(agentRef: AgentRef, forwardUri: AkkaUri, headers: Seq[HttpHeader]): Task[HttpResponse] = {
    val agentClient = AgentClient(  // TODO Reuse AgentClient of AgentDriver
      agentRef.uri,
      userAndPassword = None,
      label = agentRef.id.toString,
      controllerConfiguration.keyStoreRefOption,
      controllerConfiguration.trustStoreRefs)
    implicit val sessionToken: Task[Option[SessionToken]] = Task.pure(None)
    agentClient
      .sendReceive(HttpRequest(GET,  forwardUri, headers = headers.filter(h => isForwardableHeaderClass(h.getClass))))
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
