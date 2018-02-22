package com.sos.jobscheduler.agent.client
import akka.actor.ActorSystem
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.HttpEncodings.gzip
import akka.http.scaladsl.model.headers.{Accept, Authorization, BasicHttpCredentials, `Accept-Encoding`, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, Uri, _}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.agent.client.AgentClient._
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.CirceJsonSupport._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpClientUtils.RichHttpResponse
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.decodeResponse
import com.sos.jobscheduler.common.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.soslicense.LicenseKeyString
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.session.SessionToken
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
 * Client for JobScheduler Agent.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient extends AutoCloseable {

  protected def actorSystem: ActorSystem
  protected implicit def executionContext: ExecutionContext
  val agentUri: Uri
  protected def httpsConnectionContextOption: Option[HttpsConnectionContext]
  protected def userAndPasswordOption: Option[UserAndPassword]

  private lazy val logger = Logger.withPrefix[AgentClient](agentUri.toString)
  protected lazy val agentUris = AgentUris(agentUri.toString)
  private lazy val uriPrefixString = agentUris.prefixedUri.path.toString
  private lazy val httpsConnectionContext = httpsConnectionContextOption getOrElse http.defaultClientHttpsContext
  private lazy val credentialsHeaders: List[HttpHeader] = userAndPasswordOption match {
    case Some(UserAndPassword(UserId(user), SecretString(password))) ⇒ Authorization(BasicHttpCredentials(user, password)) :: Nil
    case None ⇒ Nil
  }
  private val sessionTokenRef = new AtomicReference[Option[SessionToken]](None)
  private implicit lazy val materializer = ActorMaterializer()(actorSystem)
  private lazy val http = Http(actorSystem)

  def close() = {
    materializer.shutdown()  // (Seems to be an asynchronous operation)
  }

  final def executeCommand(command: AgentCommand): Future[command.Response] = {
    logger.debug(s"Execute $command")
    val response = command match {
      case Login ⇒
        for (response ← executeCommand2[Login.Response](command, SessionAction.Login)) yield {
          setSessionToken(response.sessionToken)
          response
        }

      case Logout ⇒
        val tokenOption = sessionTokenRef.get
        for (response ← executeCommand2[Logout.Response](command, SessionAction.Logout(tokenOption))) yield {
          sessionTokenRef.compareAndSet(tokenOption, None)  // Changes nothing in case of a concurrent successful Login
          response
        }

      case _ ⇒
        executeCommand2[AgentCommand.Response](command)
    }
    response map { _.asInstanceOf[command.Response] }
  }

  private def executeCommand2[A: FromResponseUnmarshaller](command: AgentCommand, sessionAction: SessionAction = SessionAction.Default): Future[A] =
    for {
      entity ← Marshal(command).to[RequestEntity]
      httpResponse ← sendReceive(
        Gzip.encodeMessage(
          HttpRequest(POST, agentUris.command, Accept(`application/json`) :: Nil, entity)),
        sessionAction)
      response ← Unmarshal(httpResponse).to[A]
    } yield
      response


  final def overview: Future[AgentOverview] = get[AgentOverview](_.overview)

  object task {
    final def overview: Future[TaskRegisterOverview] = get[TaskRegisterOverview](_.task.overview)

    final def tasks: Future[immutable.Seq[TaskOverview]] = get[immutable.Seq[TaskOverview]](_.task.tasks)

    final def apply(id: AgentTaskId): Future[TaskOverview] = get[TaskOverview](_.task(id))
  }

  final def order(orderId: OrderId): Future[Order[Order.State]] =
    get[Order[Order.State]](_.order(orderId))

  final def orderIds(): Future[Seq[OrderId]] =
    get[Seq[OrderId]](_.order.ids)

  final def orders(): Future[Seq[Order[Order.State]]] =
    get[Seq[Order[Order.State]]](_.order.orders)

  final def mastersEvents(request: EventRequest[OrderEvent]): Future[EventSeq[Seq, KeyedEvent[OrderEvent]]] = {
    //TODO Use Akka http connection level request with Akka streams and .withIdleTimeout()
    // See https://gist.github.com/burakbala/49617745ead702b4c83cf89699c266ff
    //val timeout = request match {
    //  case o: EventRequest[_] ⇒ o.timeout + 10.s
    //  case _ ⇒ akka configured default value
    //}
    get[EventSeq[Seq, KeyedEvent[OrderEvent]]](_.mastersEvents(request))
  }

  final def get[A: FromResponseUnmarshaller](uri: AgentUris ⇒ Uri): Future[A] =
    getUri[A](uri(agentUris))

  final def getUri[A: FromResponseUnmarshaller](uri: Uri): Future[A] =
    sendReceive(HttpRequest(GET, uri, Accept(`application/json`) :: `Cache-Control`(`no-cache`, `no-store`) :: Nil))

  private def sendReceive[A: FromResponseUnmarshaller](request: HttpRequest, sessionAction: SessionAction = SessionAction.Default): Future[A] =
    withCheckedAgentUri(request) { request ⇒
      val req = Gzip.encodeMessage(request.copy(
        headers = sessionCredentialsHeaders(sessionAction) ::: `Accept-Encoding`(gzip) :: request.headers.toList))
      http.singleRequest(req, httpsConnectionContext) flatMap { httpResponse ⇒
        if (httpResponse.status.isSuccess)
          Unmarshal(decodeResponse(httpResponse)).to[A]
        else
          for (message ← decodeResponse(httpResponse).utf8StringFuture) yield
            throw new HttpException(httpResponse.status, message truncateWithEllipsis ErrorMessageLengthMaximum)
      }
    }

  private def sessionCredentialsHeaders(sessionAction: SessionAction): List[HttpHeader] =
    sessionAction match {
      case SessionAction.Default ⇒ sessionTokenRef.get match {
        case o: Some[SessionToken] ⇒
          sessionTokenRequestHeaderOption(o).toList  // In session: Server should have credentials (see login)
        case None ⇒
          credentialsHeaders   // No session: transfer credentials with every request
      }
      case SessionAction.Login ⇒
        sessionTokenRequestHeaderOption(sessionTokenRef.get) ++:  // Logout optional previous session
          credentialsHeaders
      case SessionAction.Logout(sessionTokenOption) ⇒
        sessionTokenRequestHeaderOption(sessionTokenOption).toList
    }

  private def sessionTokenRequestHeaderOption(sessionTokenOption: Option[SessionToken]): Option[HttpHeader] =
    sessionTokenOption map { token ⇒ makeHeader(SessionToken.HeaderName, token.secret.string) }

  private def makeHeader(name: String, value: String): HttpHeader =
    HttpHeader.parse(name, value) match {
      case HttpHeader.ParsingResult.Ok(header, Nil) ⇒ header
      case o ⇒ throw new IllegalArgumentException(o.errors.headOption map { _.summary } getOrElse s"Invalid http header: $name")
    }

  private[agent] final def setSessionToken(sessionToken: SessionToken): Unit =
    sessionTokenRef.set(Some(sessionToken))

  final def clearSession(): Unit = {
    sessionTokenRef.set(None)
  }

  final def hasSession: Boolean =
    sessionTokenRef.get.nonEmpty

  private def withCheckedAgentUri[A](request: HttpRequest)(body: HttpRequest ⇒ Future[A]): Future[A] =
    toCheckedAgentUri(request.uri) match {
      case Some(uri) ⇒ body(request.copy(uri = uri))
      case None ⇒ Future.failed(new IllegalArgumentException(s"URI '${request.uri}' does not match $toString"))
    }

  private[client] final def toCheckedAgentUri(uri: Uri): Option[Uri] =
    checkAgentUri(normalizeAgentUri(uri))

  private[client] final def normalizeAgentUri(uri: Uri): Uri = {
    val Uri(scheme, authority, path, query, fragment) = uri
    if (scheme.isEmpty && authority.isEmpty)
      Uri(agentUri.scheme, agentUri.authority, path, query, fragment)
    else
      uri
  }

  private[client] def checkAgentUri(uri: Uri): Option[Uri] = {
    uri.scheme == agentUri.scheme &&
      uri.authority == agentUri.authority &&
      (uri.path.toString startsWith uriPrefixString) option
        uri
  }

  override def toString = s"AgentClient($agentUri)"
}

object AgentClient {
  val ErrorMessageLengthMaximum = 10000

  def apply(
    agentUri: Uri,
    licenseKeys: immutable.Iterable[LicenseKeyString] = Nil,
    httpsConntextContextOption: Option[HttpsConnectionContext] = None,
    userAndPasswordOption: Option[UserAndPassword] = None)
    (implicit actorSystem: ActorSystem)
  : AgentClient =
    new Standard(actorSystem, agentUri, licenseKeys, httpsConntextContextOption, userAndPasswordOption)(actorSystem.dispatcher)

  private class Standard(
    protected val actorSystem: ActorSystem,
    val agentUri: Uri,
    protected val licenseKeys: immutable.Iterable[LicenseKeyString] = Nil,
    protected val httpsConnectionContextOption: Option[HttpsConnectionContext] = None,
    protected val userAndPasswordOption: Option[UserAndPassword] = None)
    (implicit protected val executionContext: ExecutionContext)
  extends AgentClient

  def sessionMayBeLost(t: Throwable): Boolean =
    t match {
      case t: HttpException if t.status == Unauthorized || t.status == Forbidden ⇒ true
      case _ ⇒ false
    }

  private sealed trait SessionAction
  private object SessionAction {
    case object Default extends SessionAction
    final case object Login extends SessionAction
    final case class Logout(sessionTokenOption: Option[SessionToken]) extends SessionAction
  }

  final class HttpException(val status: StatusCode, message: String) extends RuntimeException(s"$status: $message".trim)
}
