package js7.controller.web.controller.api

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods.GET
import akka.http.scaladsl.model.headers.{Accept, `Accept-Charset`, `Accept-Encoding`, `Accept-Ranges`, `Cache-Control`, `Content-Encoding`, `Content-Length`, `Content-Range`, `Content-Type`, `If-Match`, `If-Modified-Since`, `If-None-Match`, `If-Range`}
import akka.http.scaladsl.model.{HttpEntity, HttpHeader, HttpRequest, HttpResponse, headers, Uri as AkkaUri}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{PathMatchers, Route}
import com.typesafe.config.ConfigUtil
import js7.agent.client.AgentClient
import js7.base.auth.{AgentDirectorForwardPermission, UserAndPassword, ValidUserPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.http.AkkaHttpUtils.RichAkkaUri
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.AgentForwardRoute.*
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.controller.ControllerState
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.MapView

trait AgentForwardRoute extends ControllerRouteProvider
{
  protected implicit def actorSystem: ActorSystem
  protected def pathToAgentRefState: Task[Checked[MapView[AgentPath, AgentRefState]]]
  protected def controllerConfiguration: ControllerConfiguration
  protected def controllerState: Task[Checked[ControllerState]]

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final lazy val agentForwardRoute: Route =
    authorizedUser(ValidUserPermission)(_ =>
      pathPrefix(Segment)(agentPath =>
        AgentPath.checked(agentPath) match {
          case Left(problem) => complete(problem)
          case Right(agentPath) =>
            extractRequest { request =>
              val userAndPassword = controllerConfiguration.config
                .optionAs[SecretString](
                  "js7.auth.agents." + ConfigUtil.joinPath(agentPath.string))
                .map(UserAndPassword(controllerConfiguration.controllerId.toUserId, _))

              forward(agentPath, userAndPassword, request)
            }
        }))

  private def forward(
    agentPath: AgentPath,
    userAndPassword: Option[UserAndPassword],
    request: HttpRequest)
  : Route = {
    def route =
      rawPathPrefix(PathMatchers.Remaining)(remaining =>
        if (request.method == GET && remaining.isEmpty)
          forward1(remainingUrl = "")
        else
          authorizedUser(AgentDirectorForwardPermission)(_ =>
            forward1(remaining)))

    def forward1(remainingUrl: String): Route =
      completeTask(
        pathToAgentRefState.map(_.flatMap(_.checked(agentPath)))
          .flatMapT(agentRefState =>
            forward2(agentRefState.agentRef, remainingUrl = remainingUrl)
              .map(Right.apply)))

    def forward2(agentRef: AgentRef, remainingUrl: String)
    : Task[HttpResponse] =
      controllerState
        .map(_.flatMap(s =>
          s.agentToUri(agentRef.path)
            .map(uri =>
              uri -> uri.asAkka.copy(
                path = AkkaUri.Path((uri.asAkka.path ?/ "agent" / "api").toString + remainingUrl),
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

    def forwardTo(agentRef: AgentRef, uri: Uri, akkaUri: AkkaUri, headers: Seq[HttpHeader])
    : Task[HttpResponse] =
      AgentClient // TODO Reuse AgentClient of AgentDriver
        .resource(
          uri,
          userAndPassword,
          label = agentRef.path.toString,
          controllerConfiguration.httpsConfig)
        .use { agentClient =>
          implicit val sessionToken = Task(agentClient.sessionToken)
          agentClient.login() *>
            agentClient
              .sendReceive(HttpRequest(request.method, akkaUri,
                headers = headers.filter(h => isForwardableHeaderClass(h.getClass)),
                entity = request.entity))
              .map(response => response.withHeaders(
                response.headers.filter(h => !isIgnoredResponseHeader(h.getClass))))
        }

    route
  }
}

object AgentForwardRoute {
  private val isForwardableHeaderClass = Set[Class[? <: HttpHeader]](
    classOf[Accept],
    classOf[`Accept-Encoding`],
    classOf[`Accept-Charset`],
    classOf[`Accept-Ranges`],
    classOf[`Content-Range`],
    classOf[`Content-Type`],
    classOf[`Content-Length`],
    classOf[`Content-Encoding`],
    classOf[`If-Range`],
    classOf[`If-Match`],
    classOf[`If-None-Match`],
    classOf[`If-Modified-Since`],
    classOf[`Cache-Control`])

  private val isIgnoredResponseHeader = Set[Class[? <: HttpHeader]](
    //classOf[headers.Server],
    //classOf[headers.Date],
    classOf[headers.`Cache-Control`],
    classOf[headers.`Tls-Session-Info`])
    //classOf[headers.`Content-Type`],
    //classOf[headers.`Content-Length`])
}
