package com.sos.jobscheduler.master.web.master.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, headers, Uri => AkkaUri}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.base.auth.{SessionToken, ValidUserPermission}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.completeTask
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichAkkaUri
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.AgentProxyRoute._
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
      masterConfiguration.trustStoreRefOption)
    implicit val sessionToken: Option[SessionToken] = None
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
