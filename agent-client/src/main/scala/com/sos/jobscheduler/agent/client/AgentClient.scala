package com.sos.jobscheduler.agent.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.data.views.{AgentOverview, TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.agent.data.{AgentApi, AgentTaskId}
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.https.AkkaHttps.loadHttpsConnectionContext
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
 * Client for JobScheduler Agent.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient extends AgentApi with SessionApi with AkkaHttpClient {

  protected def httpClient = this

  protected def keyStoreRef: Option[KeyStoreRef]
  protected def trustStoreRef: Option[TrustStoreRef]

  override protected lazy val httpsConnectionContextOption =
    (keyStoreRef.nonEmpty || trustStoreRef.nonEmpty) ? loadHttpsConnectionContext(keyStoreRef, trustStoreRef)  // TODO None means HttpsConnectionContext? Or empty context?

  protected lazy val sessionUri = agentUris.session.toString()
  protected lazy val agentUris = AgentUris(baseUri.toString)
  protected lazy val uriPrefixPath = "/agent"

  final def commandExecute(command: AgentCommand): Task[command.Response] =
    post[AgentCommand, AgentCommand.Response](uri = agentUris.command.toString, command)
      .map(_.asInstanceOf[command.Response])

  final def overview: Task[AgentOverview] = get[AgentOverview](agentUris.overview)

  object task {
    final def overview: Task[TaskRegisterOverview] = get[TaskRegisterOverview](agentUris.task.overview)

    final def tasks: Task[Seq[TaskOverview]] = get[Seq[TaskOverview]](agentUris.task.tasks)

    final def apply(id: AgentTaskId): Task[TaskOverview] = get[TaskOverview](agentUris.task(id))
  }

  final def order(orderId: OrderId): Task[Order[Order.State]] =
    get[Order[Order.State]](agentUris.order(orderId))

  final def orderIds: Task[Seq[OrderId]] =
    get[Seq[OrderId]](agentUris.order.ids)

  final def orders: Task[Seq[Order[Order.State]]] =
    get[Seq[Order[Order.State]]](agentUris.order.orders)

  final def mastersEvents[E <: Event](request: EventRequest[E]): Task[TearableEventSeq[Seq, KeyedEvent[E]]] = {
    //TODO Use Akka http connection level request with Akka streams and .withIdleTimeout()
    // See https://gist.github.com/burakbala/49617745ead702b4c83cf89699c266ff
    //val timeout = request match {
    //  case o: EventRequest[_] ⇒ o.timeout + 10.s
    //  case _ ⇒ akka configured default value
    //}
    get[TearableEventSeq[Seq, KeyedEvent[E]]](agentUris.mastersEvents(request))
  }

  override def toString = s"AgentClient($baseUri)"
}

object AgentClient {
  val ErrorMessageLengthMaximum = 10000

  def apply(agentUri: Uri, keyStoreRef: ⇒ Option[KeyStoreRef] = None, trustStoreRef: ⇒ Option[TrustStoreRef] = None)(implicit actorSystem: ActorSystem): AgentClient = {
    val a = actorSystem
    def k = keyStoreRef    // lazy, to avoid reference when not needed (needed only for http)
    def t = trustStoreRef  // lazy, to avoid reference when not needed (needed only for http)
    new AgentClient {
      protected val actorSystem = a
      val baseUri = agentUri
      protected def keyStoreRef = k
      protected def trustStoreRef = t
    }
  }
}
