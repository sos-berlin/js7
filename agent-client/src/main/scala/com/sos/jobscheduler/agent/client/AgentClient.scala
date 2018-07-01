package com.sos.jobscheduler.agent.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.data.views.{AgentOverview, TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeyStoreRef}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import monix.eval.Task
import scala.collection.immutable
import scala.collection.immutable.Seq

/**
 * Client for JobScheduler Agent.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient extends SessionApi with AkkaHttpClient {

  protected def httpClient = this

  protected def keyStoreRef: Option[KeyStoreRef]

  override protected lazy val httpsConnectionContextOption =
    keyStoreRef map Https.loadHttpsConnectionContext

  protected lazy val sessionUri = agentUris.session.toString()
  protected lazy val agentUris = AgentUris(baseUri.toString)
  protected lazy val uriPrefixPath = "/agent"

  final def executeCommand(command: AgentCommand): Task[command.Response] =
    post[AgentCommand, AgentCommand.Response](agentUris.command, command)
      .map(_.asInstanceOf[command.Response])

  final def overview: Task[AgentOverview] = get[AgentOverview](agentUris.overview)

  object task {
    final def overview: Task[TaskRegisterOverview] = get[TaskRegisterOverview](agentUris.task.overview)

    final def tasks: Task[immutable.Seq[TaskOverview]] = get[immutable.Seq[TaskOverview]](agentUris.task.tasks)

    final def apply(id: AgentTaskId): Task[TaskOverview] = get[TaskOverview](agentUris.task(id))
  }

  final def order(orderId: OrderId): Task[Order[Order.State]] =
    get[Order[Order.State]](agentUris.order(orderId))

  final def orderIds(): Task[Seq[OrderId]] =
    get[Seq[OrderId]](agentUris.order.ids)

  final def orders(): Task[Seq[Order[Order.State]]] =
    get[Seq[Order[Order.State]]](agentUris.order.orders)

  final def mastersEvents(request: EventRequest[OrderEvent]): Task[EventSeq[Seq, KeyedEvent[OrderEvent]]] = {
    //TODO Use Akka http connection level request with Akka streams and .withIdleTimeout()
    // See https://gist.github.com/burakbala/49617745ead702b4c83cf89699c266ff
    //val timeout = request match {
    //  case o: EventRequest[_] ⇒ o.timeout + 10.s
    //  case _ ⇒ akka configured default value
    //}
    get[EventSeq[Seq, KeyedEvent[OrderEvent]]](agentUris.mastersEvents(request))
  }

  override def toString = s"AgentClient($baseUri)"
}

object AgentClient {
  val ErrorMessageLengthMaximum = 10000

  def apply(agentUri: Uri, keyStoreRef: Option[KeyStoreRef] = None)(implicit actorSystem: ActorSystem): AgentClient =
    new Standard(actorSystem, agentUri, keyStoreRef)

  private class Standard(
    protected val actorSystem: ActorSystem,
    val baseUri: Uri,
    protected val keyStoreRef: Option[KeyStoreRef] = None)
  extends AgentClient
}
