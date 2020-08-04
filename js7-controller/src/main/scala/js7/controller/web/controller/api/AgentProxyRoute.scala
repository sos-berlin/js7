package js7.controller.web.controller.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, headers, Uri => AkkaUri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.agent.client.AgentClient
import js7.base.auth.{SessionToken, ValidUserPermission}
import js7.base.problem.Checked._
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.StandardMarshallers._
import js7.common.http.AkkaHttpUtils.RichAkkaUri
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.AgentProxyRoute._
import js7.core.item.InventoryItemApi
import js7.data.agent.{AgentRef, AgentRefPath}
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentProxyRoute extends ControllerRouteProvider
{
  protected implicit def actorSystem: ActorSystem
  protected def itemApi: InventoryItemApi
  protected def controllerConfiguration: ControllerConfiguration

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentProxyRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        path(Segment) { pathString =>
          extractRequest { request =>
            completeTask(
              for {
                checkedAgent <- itemApi.pathToCurrentItem[AgentRef](AgentRefPath(s"/$pathString"))
                checkedResponse <- checkedAgent.map(forward(_, request)).evert
              } yield checkedResponse)
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
    forwardTo(agentRef.uri, uri, request.headers)
  }

  private def forwardTo(agentUri: Uri, forwardUri: AkkaUri, headers: Seq[HttpHeader]): Task[HttpResponse] = {
    val agentClient = AgentClient(  // TODO Reuse AgentClient of AgentDriver
      agentUri,
      userAndPassword = None,
      controllerConfiguration.keyStoreRefOption,
      controllerConfiguration.trustStoreRefs)
    implicit val sessionToken: Task[Option[SessionToken]] = Task.pure(None)
    agentClient
      .sendReceive(HttpRequest(GET,  forwardUri, headers = headers filter { h => isForwardableHeaderClass(h.getClass) }))
      .map(response => response.withHeaders(response.headers filterNot { h => IsIgnoredAgentHeader(h.getClass) }))
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
