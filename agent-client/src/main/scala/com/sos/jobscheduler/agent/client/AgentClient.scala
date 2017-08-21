package com.sos.jobscheduler.agent.client

import akka.actor.ActorRefFactory
import akka.util.Timeout
import com.sos.jobscheduler.agent.client.AgentClient._
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.commandresponses.{EmptyResponse, LoginResponse}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.views.{TaskRegisterOverview, TaskOverview}
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.agent.scheduler.event.KeyedEventJsonFormats.keyedEventJsonFormat
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.soslicense.LicenseKeyString
import com.sos.jobscheduler.common.sprayutils.sprayclient.ExtendedPipelining.extendedSendReceive
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.IntelliJUtils.intelliJuseImports
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.session.SessionToken
import java.time.Duration
import java.util.concurrent.atomic.AtomicReference
import org.jetbrains.annotations.TestOnly
import org.scalactic.Requirements._
import scala.collection.immutable
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import spray.can.Http.HostConnectorSetup
import spray.client.pipelining._
import spray.http.CacheDirectives.{`no-cache`, `no-store`}
import spray.http.HttpHeaders.{Accept, `Cache-Control`}
import spray.http.MediaTypes._
import spray.http.StatusCodes.{Forbidden, InternalServerError, Unauthorized}
import spray.http._
import spray.httpx.SprayJsonSupport._
import spray.httpx.UnsuccessfulResponseException
import spray.httpx.encoding.Gzip
import spray.httpx.unmarshalling._
import spray.json.DefaultJsonProtocol._

/**
 * Client for JobScheduler Agent.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient {
  import actorRefFactory.dispatcher

  val agentUri: Uri
  protected def licenseKeys: immutable.Iterable[LicenseKeyString]
  implicit protected val actorRefFactory: ActorRefFactory
  protected def hostConnectorSetupOption: Option[HostConnectorSetup]
  protected def userAndPasswordOption: Option[UserAndPassword]

  private lazy val logger = Logger.withPrefix[AgentClient](agentUri.toString)
  protected lazy val agentUris = AgentUris(agentUri.toString)
  private lazy val addLicenseKeys: RequestTransformer = if (licenseKeys.nonEmpty) addHeader(AgentUris.LicenseKeyHeaderName, licenseKeys mkString " ")
    else identity
  private lazy val addUserAndPassword: RequestTransformer = userAndPasswordOption match {
    case Some(UserAndPassword(UserId(user), SecretString(password))) ⇒ addCredentials(BasicHttpCredentials(user, password))
    case None ⇒ identity
  }
  private val sessionTokenRef = new AtomicReference[Option[SessionToken]](None)

  final def executeCommand(command: AgentCommand): Future[command.Response] = {
    logger.debug(s"Execute $command")
    val response = command match {
      case Login ⇒
        for (response ← unmarshallingPipeline[LoginResponse](RequestTimeout, SessionAction.Login).apply(Post(agentUris.command, command: AgentCommand))) yield {
          setSessionToken(response.sessionToken)
          response
        }
      case Logout ⇒
        val tokenOption = sessionTokenRef.get
        val whenResponded = unmarshallingPipeline[EmptyResponse.type](RequestTimeout, SessionAction.Logout(tokenOption))
          .apply(Post(agentUris.command, command: AgentCommand))
        for (response ← whenResponded) yield {
          sessionTokenRef.compareAndSet(tokenOption, None)  // Changes nothing in case of a concurrent successful Login
          response
        }
      case (Logout | NoOperation | _: OrderCommand | RegisterAsMaster | _: TerminateOrAbort) ⇒
        unmarshallingPipeline[EmptyResponse.type](RequestTimeout).apply(Post(agentUris.command, command: AgentCommand))
    }
    response map { _.asInstanceOf[command.Response] } recover {
      case e: UnsuccessfulResponseException if e.response.status == InternalServerError ⇒
        import e.response.entity
        val message = if (entity.data.length < 1024) entity.asString else entity.data.length + " bytes"
        throw new RuntimeException(s"HTTP-${e.response.status}: $message")
    }
  }

  private object task {
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
    val timeout = request match {
      case o: EventRequest[_] ⇒ o.timeout + 10.s
      case _ ⇒ RequestTimeout
    }
    get[EventSeq[Seq, KeyedEvent[OrderEvent]]](_.mastersEvents(request), timeout)
  }

  final def get[A: FromResponseUnmarshaller](uri: AgentUris ⇒ String, timeout: Duration = RequestTimeout): Future[A] =
    unmarshallingPipeline[A](timeout).apply(Get(uri(agentUris)))

  @TestOnly
  private[sos] final def sendReceive[A: FromResponseUnmarshaller](request: HttpRequest, timeout: Duration = RequestTimeout): Future[A] =
    withCheckedAgentUri(request) { request ⇒
      unmarshallingPipeline[A](timeout).apply(request)
    }

  final def sendReceiveWithHeaders[A: FromResponseUnmarshaller]
    (request: HttpRequest, headers: List[HttpHeader], timeout: Duration = RequestTimeout)
  : Future[A] =
    withCheckedAgentUri(request) { request ⇒
      (addHeaders(headers) ~> httpResponsePipeline(timeout) ~> unmarshal[A]).apply(request)
    }

  private def unmarshallingPipeline[A: FromResponseUnmarshaller](timeout: Duration, sessionAction: SessionAction = SessionAction.Default) =
    addHeader(Accept(`application/json`)) ~>
      addHeader(`Cache-Control`(`no-cache`, `no-store`)) ~>   // Unnecessary ?
      httpResponsePipeline(timeout, sessionAction) ~>
      unmarshal[A]

  private def httpResponsePipeline(timeout: Duration, sessionAction: SessionAction = SessionAction.Default): HttpRequest ⇒ Future[HttpResponse] =
    agentSendReceive(timeout.toFiniteDuration, sessionAction)

  private[sos] def agentSendReceive(futureTimeout: Timeout)(implicit ec: ExecutionContext): SendReceive =
    agentSendReceive(futureTimeout, SessionAction.Default)

  private def agentSendReceive(futureTimeout: Timeout, sessionAction: SessionAction)(implicit ec: ExecutionContext): SendReceive = {
    addSessionCredentials(sessionAction) ~>
      encode(Gzip) ~>
      extendedSendReceive(futureTimeout, hostConnectorSetupOption)(actorRefFactory, ec) ~>
      decode(Gzip)
  }

  private def addSessionCredentials(sessionAction: SessionAction): RequestTransformer =
    sessionAction match {
      case SessionAction.Default ⇒ sessionTokenRef.get match {
        case o: Some[SessionToken] ⇒
          sessionTokenRequestTransformer(o)  // In session: Server should have credentials (see login)
        case None ⇒
          addUserAndPassword ~> addLicenseKeys  // No session: transfer credentials with every request
      }
      case SessionAction.Login ⇒
        sessionTokenRequestTransformer(sessionTokenRef.get) ~>  // Logout optional previous session
          addUserAndPassword ~> addLicenseKeys
      case SessionAction.Logout(sessionTokenOption) ⇒
        sessionTokenRequestTransformer(sessionTokenOption)
    }

  private def sessionTokenRequestTransformer(sessionTokenOption: Option[SessionToken]): RequestTransformer =
    sessionTokenOption map { token ⇒ addHeader(SessionToken.HeaderName, token.secret.string) } getOrElse identity

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
      case None ⇒ Future.failed(new IllegalArgumentException(s"URI '${request.uri} does not match $toString"))
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
    val myAgentUri = Uri(scheme = uri.scheme, authority = uri.authority)
    myAgentUri == agentUri && uri.path.toString.startsWith(agentUris.prefixedUri.path.toString) option
      uri
  }

  override def toString = s"AgentClient($agentUri)"
}

object AgentClient {
  intelliJuseImports(StringJsonFormat)  // for import spray.json.DefaultJsonProtocol._

  val RequestTimeout = 60.s
  //private val RequestTimeoutMaximum = Int.MaxValue.ms  // Limit is the number of Akka ticks, where a tick can be as short as 1ms (see akka.actor.LightArrayRevolverScheduler.checkMaxDelay)

  def apply(
    agentUri: Uri,
    licenseKeys: immutable.Iterable[LicenseKeyString] = Nil,
    hostConnectorSetupOption: Option[HostConnectorSetup] = None,
    userAndPasswordOption: Option[UserAndPassword] = None)
    (implicit actorRefFactory: ActorRefFactory)
  : AgentClient =
    new Standard(agentUri, licenseKeys, hostConnectorSetupOption, userAndPasswordOption)

  private class Standard(
    val agentUri: Uri,
    protected val licenseKeys: immutable.Iterable[LicenseKeyString] = Nil,
    protected val hostConnectorSetupOption: Option[HostConnectorSetup] = None,
    protected val userAndPasswordOption: Option[UserAndPassword] = None)
    (implicit protected val actorRefFactory: ActorRefFactory)
  extends AgentClient

  /**
   * The returns timeout for the HTTP request is longer than the expected duration of the request
   */
  private[agent] def commandDurationToRequestTimeout(duration: Duration): Timeout = {
    require(duration >= 0.s)
    Timeout((duration + RequestTimeout).toFiniteDuration)
  }

  def sessionIsPossiblyLost(t: Throwable): Boolean =
    t match {
      case t: UnsuccessfulResponseException if t.response.status == Unauthorized || t.response.status == Forbidden ⇒ true
      case _ ⇒ false
    }

  private sealed trait SessionAction
  private object SessionAction {
    case object Default extends SessionAction
    final case object Login extends SessionAction
    final case class Logout(sessionTokenOption: Option[SessionToken]) extends SessionAction
  }
}
