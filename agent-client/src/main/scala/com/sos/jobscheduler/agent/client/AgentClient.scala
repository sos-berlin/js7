package com.sos.jobscheduler.agent.client

import akka.actor.ActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.keyedEventJsonCodec
import com.sos.jobscheduler.agent.data.views.{AgentOverview, TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.data.web.AgentUris
import com.sos.jobscheduler.agent.data.{AgentApi, AgentTaskId}
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.session.HttpSessionApi
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import monix.eval.Task
import monix.reactive.Observable

/**
 * Client for JobScheduler Agent Server.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient extends AgentApi with HttpSessionApi with AkkaHttpClient
{
  protected def httpClient = this

  def baseUri: Uri
  protected def keyStoreRef: Option[KeyStoreRef]
  protected def trustStoreRef: Option[TrustStoreRef]

  protected lazy val sessionUri = agentUris.session
  protected lazy val agentUris = AgentUris(baseUri)
  protected lazy val uriPrefixPath = "/agent"

  final def commandExecute(command: AgentCommand): Task[Checked[command.Response]] =
    liftProblem(
      post[AgentCommand, AgentCommand.Response](uri = agentUris.command, command)
        .map(_.asInstanceOf[command.Response]))

  final def overview: Task[AgentOverview] = get[AgentOverview](agentUris.overview)

  object task {
    final def overview: Task[TaskRegisterOverview] = get[TaskRegisterOverview](agentUris.task.overview)

    final def tasks: Task[Seq[TaskOverview]] = get[Seq[TaskOverview]](agentUris.task.tasks)

    final def apply(id: AgentTaskId): Task[TaskOverview] = get[TaskOverview](agentUris.task(id))
  }

  final def order(orderId: OrderId): Task[Checked[Order[Order.State]]] =
    liftProblem(
      get[Order[Order.State]](agentUris.order(orderId)))

  final def orderIds: Task[Checked[Seq[OrderId]]] =
    liftProblem(
      get[Seq[OrderId]](agentUris.order.ids))

  final def orders: Task[Checked[Seq[Order[Order.State]]]] =
    liftProblem(
      get[Seq[Order[Order.State]]](agentUris.order.orders))

  final def mastersEventObservable(request: EventRequest[Event]): Task[Checked[Observable[Stamped[KeyedEvent[Event]]]]] =
    liftProblem(
      getDecodedLinesObservable[Stamped[KeyedEvent[Event]]](agentUris.mastersEvents(request)))

  final def mastersEvents(request: EventRequest[Event]): Task[Checked[TearableEventSeq[Seq, KeyedEvent[Event]]]] = {
    //TODO Use Akka http connection level request with Akka streams and .withIdleTimeout()
    // See https://gist.github.com/burakbala/49617745ead702b4c83cf89699c266ff
    //val timeout = request match {
    //  case o: EventRequest[_] => o.timeout + 10.s
    //  case _ => akka configured default value
    //}
    liftProblem(
      get[TearableEventSeq[Seq, KeyedEvent[Event]]](agentUris.mastersEvents(request)))
  }

  override def toString = baseUri.toString
}

object AgentClient
{
  def apply(agentUri: Uri, userAndPassword: Option[UserAndPassword],
    keyStoreRef: => Option[KeyStoreRef] = None, trustStoreRef: => Option[TrustStoreRef] = None)
    (implicit actorSystem: ActorSystem)
  : AgentClient = {
    val a = actorSystem
    val up = userAndPassword
    def k = keyStoreRef    // lazy, to avoid reference when not needed (needed only for http)
    def t = trustStoreRef  // lazy, to avoid reference when not needed (needed only for http)
    new AgentClient {
      protected val actorSystem = a
      val baseUri = agentUri
      protected val name = "Agent"
      protected def keyStoreRef = k
      protected def trustStoreRef = t
      protected def userAndPassword = up
    }
  }
}
