package js7.master.web.master.api

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
import js7.core.filebased.FileBasedApi
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.master.configuration.MasterConfiguration
import js7.master.web.common.MasterRouteProvider
import js7.master.web.master.api.AgentProxyRoute._
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
trait AgentProxyRoute extends MasterRouteProvider
{
  protected implicit def actorSystem: ActorSystem
  protected def fileBasedApi: FileBasedApi
  protected def masterConfiguration: MasterConfiguration

  private implicit def implicitScheduler: Scheduler = scheduler

  final val agentProxyRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        path(Segment) { pathString =>
          extractRequest { request =>
            completeTask(
              for {
                checkedAgent <- fileBasedApi.pathToCurrentFileBased[AgentRef](AgentRefPath(s"/$pathString"))
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
      masterConfiguration.keyStoreRefOption,
      masterConfiguration.trustStoreRefs)
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
