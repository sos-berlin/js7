package com.sos.jobscheduler.master.web.master.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, Uri, headers}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.monix.MonixForCats._
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.AgentProxyRoute._
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait AgentProxyRoute extends MasterRouteProvider
{
  protected implicit def scheduler: Scheduler
  protected implicit def actorSystem: ActorSystem
  protected def fileBasedApi: FileBasedApi
  protected def masterConfiguration: MasterConfiguration

  final val agentProxyRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ ⇒
        path(Segment) { pathString ⇒
          extractRequest { request ⇒
            complete(
              for {
                checkedAgent ← fileBasedApi.pathToCurrentFileBased[Agent](AgentPath(s"/$pathString"))
                checkedResponse ← checkedAgent.map(stampedAgent ⇒ forward(stampedAgent, request)).evert
              } yield checkedResponse)
          }
        }
      }
    }

  private def forward(stampedAgent: Stamped[Agent], request: HttpRequest): Task[HttpResponse] = {
    val agent = stampedAgent.value
    val agentUri = Uri(agent.uri)
    val uri = agentUri.copy(
      path = Uri.Path((agentUri.path ?/ "agent" / "api").toString),
      rawQueryString = request.uri.rawQueryString)
    forwardTo(agent.uri, uri, request.headers)
  }

  private def forwardTo(agentUri: Uri, forwardUri: Uri, headers: Seq[HttpHeader]): Task[HttpResponse] = {
    val agentClient = AgentClient(agentUri, masterConfiguration.keyStoreRef.toOption)  // TODO Reuse AgentClient of AgentDriver
    agentClient.sendReceive(
      HttpRequest(GET,  forwardUri, headers = headers filter { h ⇒ isForwardableHeaderClass(h.getClass) }))
        .map(response ⇒ response.withHeaders(response.headers filterNot { h ⇒ IsIgnoredAgentHeader(h.getClass) }))
      .doOnFinish(_ ⇒ Task {
        agentClient.close()
      })
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
