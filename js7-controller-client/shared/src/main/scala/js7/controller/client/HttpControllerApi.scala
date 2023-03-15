package js7.controller.client

import cats.effect.Resource
import io.circe.{Decoder, Encoder, Json}
import izumi.reflect.Tag
import js7.agent.data.commands.AgentCommand
import js7.base.auth.UserAndPassword
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.session.SessionApi
import js7.base.web.{HttpClient, Uri}
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.controller.client.HttpControllerApi.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.controller.{ControllerCommand, ControllerOverview, ControllerState}
import js7.data.event.{EventApi, EventId, JournalInfo}
import js7.data.order.{FreshOrder, OrderId, OrdersOverview}
import js7.data.session.HttpSessionApi
import monix.eval.Task
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

trait HttpControllerApi
extends EventApi with HttpClusterNodeApi with HttpSessionApi with HasIsIgnorableStackTrace
{
  type State = ControllerState

  def httpClient: HttpClient
  /** Host URI or empty for addressing base on "controller/" */
  def baseUri: Uri

  protected final def prefixedUri = baseUri / UriPrefixPath

  //protected final def sessionUri = uris.session

  private final lazy val uris = ControllerUris(
    controllerUri =
      if (baseUri.isEmpty) baseUri
      else Uri(baseUri.string.stripSuffix("/") + "/controller"))

  final def post[A: Encoder, B: Decoder](uriTail: String, data: A): Task[B] =
    httpClient.post[A, B](baseUri /? uriTail, data)

  final def postObservable[A: Encoder: Tag, B: Decoder](uriTail: String, data: Observable[A]): Task[B] =
    httpClient.postObservable[A, B](baseUri /? uriTail, data)

  @TestOnly
  final def postObservableJsonString(uriTail: String, data: Observable[String]): Task[Json] =
    httpClient.postObservableJsonString(baseUri /? uriTail, data)

  final def get[B: Decoder](uriTail: String): Task[B] =
    httpClient.get[B](baseUri /? uriTail)

  final def executeCommand(command: ControllerCommand): Task[command.Response] =
    httpClient.post[ControllerCommand, ControllerCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.Response])

  final def executeAgentCommand(agentPath: AgentPath, command: AgentCommand)
  : Task[command.Response] =
    httpClient.post[AgentCommand, AgentCommand.Response](uris.agentCommand(agentPath), command)
      .map(_.asInstanceOf[command.Response])

  final def overview: Task[ControllerOverview] =
    httpClient.get[ControllerOverview](uris.overview)

  final def addOrder(order: FreshOrder): Task[Boolean] = {
    val uri = uris.order.add
    httpClient.postDiscardResponse(uri, order, allowedStatusCodes = Set(409))
      .map(_ == 201/*Created*/)
  }

  final def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    httpClient.postDiscardResponse(uris.order.add, orders)
      .map((_: Int) => Completed)

  final def deleteOrdersWhenTerminated(orderIds: Seq[OrderId]): Task[Completed] =
    executeCommand(DeleteOrdersWhenTerminated(orderIds))
      .map((_: ControllerCommand.Response.Accepted) => Completed)

  final def ordersOverview: Task[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def journalInfo: Task[JournalInfo] =
    httpClient.get[JournalInfo](uris.api("/journalInfo"))

  override def toString = s"HttpControllerApi($baseUri)"

  final def snapshot(eventId: Option[EventId] = None): Task[ControllerState] =
    snapshotAs[ControllerState](uris.snapshot.list(eventId))
}

object HttpControllerApi
{
  val UriPrefixPath = "/controller"

  /** Logs out when the resource is being released. */
  def resource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    httpClient: HttpClient,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _)
  : Resource[Task, HttpControllerApi] =
    SessionApi.resource(Task(
      new HttpControllerApi.Standard(uri, userAndPassword, httpClient, loginDelays)))

  final class Standard(
    val baseUri: Uri,
    protected val userAndPassword: Option[UserAndPassword],
    val httpClient: HttpClient,
    override protected val loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _)
  extends HttpControllerApi
}
