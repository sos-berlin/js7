package js7.controller.web.controller.api

import com.typesafe.config.ConfigUtil
import js7.agent.client.AgentClient
import js7.base.auth.{Admission, SessionToken, UserAndPassword, ValidUserPermission}
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.common.http.PekkoHttpUtils.RichPekkoUri
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.StandardMarshallers.*
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.AgentForwardRoute.*
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.controller.ControllerState
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.HttpMethods.GET
import org.apache.pekko.http.scaladsl.model.StatusCodes.Forbidden
import org.apache.pekko.http.scaladsl.model.headers.{Accept, `Accept-Charset`, `Accept-Encoding`, `Accept-Ranges`, `Cache-Control`, `Content-Encoding`, `Content-Length`, `Content-Range`, `Content-Type`, `If-Match`, `If-Modified-Since`, `If-None-Match`, `If-Range`}
import org.apache.pekko.http.scaladsl.model.{HttpEntity, HttpHeader, HttpRequest, HttpResponse, headers, Uri as PekkoUri}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{PathMatchers, Route}
import scala.collection.MapView

trait AgentForwardRoute extends ControllerRouteProvider:

  protected implicit def actorSystem: ActorSystem
  protected def pathToAgentRefState: IO[Checked[MapView[AgentPath, AgentRefState]]]
  protected def controllerConfiguration: ControllerConfiguration
  protected def controllerState: IO[Checked[ControllerState]]

  private given IORuntime = ioRuntime

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
  : Route =
    def route =
      rawPathPrefix(PathMatchers.Remaining)(remaining =>
        if request.method == GET && remaining.isEmpty then
          forward1(remainingUrl = "")
        else
          //authorizedUser(AgentDirectorForwardPermission)(_ =>
          //  forward1(remaining))
          complete(Forbidden))

    def forward1(remainingUrl: String): Route =
      completeIO(
        pathToAgentRefState.map(_.flatMap(_.checked(agentPath)))
          .flatMapT(agentRefState =>
            forward2(agentRefState.agentRef, remainingUrl = remainingUrl)
              .map(Right.apply)))

    def forward2(agentRef: AgentRef, remainingUrl: String)
    : IO[HttpResponse] =
      controllerState
        .map(_.map { s =>
          val uri = s.agentToUris(agentRef.path).head // FIXME Use AgentDriver and select the active Director
          val pekkoUri = uri.asPekko.copy(
            path = PekkoUri.Path((uri.asPekko.path ?/ "agent" / "api").toString + remainingUrl),
            rawQueryString = request.uri.rawQueryString)
          uri -> pekkoUri
        })
        .flatMap:
          case Left(problem) =>
            IO.pure(HttpResponse(
              problem.httpStatusCode,
              // Encoding: application/json, or use standard marshaller ???
              entity = HttpEntity(problem.toString)))

          case Right((uri, pekkoUri)) =>
            forwardTo(agentRef, uri, pekkoUri, request.headers)

    def forwardTo(agentRef: AgentRef, uri: Uri, pekkoUri: PekkoUri, headers: Seq[HttpHeader])
    : IO[HttpResponse] =
      AgentClient // TODO Reuse AgentClient of AgentDriver
        .resource(
          Admission(uri, userAndPassword),
          label = agentRef.path.toString,
          controllerConfiguration.httpsConfig)
        .use { agentClient =>
          implicit val sessionToken: IO[Option[SessionToken]] = IO(agentClient.sessionToken)
          agentClient.login() *>
            agentClient
              .sendReceive(HttpRequest(request.method, pekkoUri,
                headers = headers.filter(h => isForwardableHeaderClass(h.getClass)),
                entity = request.entity))
              .map(response => response.withHeaders(
                response.headers.filter(h => !isIgnoredResponseHeader(h.getClass))))
        }

    route


object AgentForwardRoute:
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
