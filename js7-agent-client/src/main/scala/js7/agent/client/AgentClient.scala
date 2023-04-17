package js7.agent.client

import akka.actor.ActorSystem
import cats.effect.Resource
import js7.agent.data.AgentState.keyedEventJsonCodec
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.*
import js7.agent.data.views.AgentOverview
import js7.agent.data.web.AgentUris
import js7.base.auth.UserAndPassword
import js7.base.io.https.HttpsConfig
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.web.Uri
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.http.AkkaHttpClient
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
import monix.eval.Task
import monix.reactive.Observable

/**
 * Client for JS7 Agent.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient
extends HttpSessionApi with AkkaHttpClient
with SessionApi.HasUserAndPassword
with HttpClusterNodeApi
{
  def httpClient = this

  def baseUri: Uri

  //protected lazy val sessionUri = agentUris.session
  protected lazy val agentUris = AgentUris(baseUri)
  protected lazy val uriPrefixPath = "/agent"

  final def commandExecute(command: AgentCommand): Task[Checked[command.Response]] =
    liftProblem(
      post[AgentCommand, AgentCommand.Response](uri = agentUris.command, command)
        .map(_.asInstanceOf[command.Response]))

  final def overview: Task[AgentOverview] = get[AgentOverview](agentUris.overview)

  final def eventObservable(request: EventRequest[Event])
  : Task[Checked[Observable[Stamped[KeyedEvent[Event]]]]] =
    liftProblem(
      getDecodedLinesObservable[Stamped[KeyedEvent[Event]]](
        agentUris.controllersEvents(request),
        responsive = true))
}

object AgentClient
{
  def apply(agentUri: Uri, userAndPassword: Option[UserAndPassword], label: String = "Agent",
    httpsConfig: => HttpsConfig = HttpsConfig.empty)
    (implicit actorSystem: ActorSystem)
  : AgentClient = {
    val a = actorSystem
    val up = userAndPassword
    def h = httpsConfig  // lazy, to avoid reference when not needed (needed only for https)
    new AgentClient with HttpClusterNodeApi  {
      override def close(): Unit = {
        logOpenSession()
        super.close()
      }

      protected val actorSystem = a
      val baseUri = agentUri
      protected val name = label
      protected def httpsConfig = h
      protected def userAndPassword = up

      protected val prefixedUri = agentUri / "agent"

      override def toString = s"AgentClient($prefixedUri)"
    }
  }

  def resource(agentUri: Uri, userAndPassword: Option[UserAndPassword], label: String = "Agent",
    httpsConfig: => HttpsConfig = HttpsConfig.empty)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AgentClient] =
    Resource.make(
      acquire = Task(apply(agentUri, userAndPassword, label, httpsConfig)))(
      release = client => client.tryLogout *> Task(client.close()))
}
