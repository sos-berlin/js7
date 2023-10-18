package js7.agent.client

import cats.effect.Resource
import js7.agent.client.AgentClient.*
import js7.agent.data.AgentState.keyedEventJsonCodec
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.*
import js7.agent.data.views.AgentOverview
import js7.agent.data.web.AgentUris
import js7.base.auth.Admission
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.web.Uri
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.http.PekkoHttpClient
import js7.data.Problems.ClusterNodeIsNotReadyProblem
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
import monix.eval.Task
import monix.reactive.Observable
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

/**
 * Client for JS7 Agent.
 * The HTTP requests are considerd to be responded within `RequestTimeout`.
 *
 * @author Joacim Zschimmer
 */
trait AgentClient
extends HttpSessionApi with PekkoHttpClient
with SessionApi.HasUserAndPassword
with HttpClusterNodeApi:

  def httpClient = this

  def baseUri: Uri

  //protected lazy val sessionUri = agentUris.session
  protected lazy val agentUris = AgentUris(baseUri)
  protected lazy val uriPrefixPath = "/agent"

  final def repeatUntilAvailable[A](timeout: FiniteDuration)(body: Task[Checked[A]])
  : Task[Checked[A]] =
    Task.defer:
      val sym = new BlockingSymbol
      val delays = Iterator(100.ms, 300.ms, 600.ms) ++ Iterator.continually(1.s)
      val until = now + timeout
      Task.tailRecM(()) { _ =>
        body
          .flatMap:
            case Left(problem) if (problem is ClusterNodeIsNotReadyProblem) && now < until =>
              sym.increment()
              logger.log(sym.logLevel, s"$sym $toString: $problem")
              Task.sleep(delays.next()).as(Left(()))

            case checked  =>
              logger.log(
                sym.releasedLogLevel,
                checked match {
                  case Left(problem) => s"❓$toString: $problem"
                  case Right(_) => s"🟢 $toString was available again"
                })
              Task.right(checked)
          .tapError(throwable => Task(
            logger.log(sym.releasedLogLevel, s"💥$toString => $throwable")))
      }

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


object AgentClient:
  private val logger = Logger[this.type]

  def apply(admission: Admission, label: String = "Agent",
    httpsConfig: => HttpsConfig = HttpsConfig.empty)
    (implicit actorSystem: ActorSystem)
  : AgentClient =
    val a = actorSystem
    val up = admission.userAndPassword
    def h = httpsConfig  // lazy, to avoid reference when not needed (needed only for https)
    new AgentClient with HttpClusterNodeApi :
      override def close(): Unit =
        logOpenSession()
        super.close()

      protected val actorSystem = a
      val baseUri = admission.uri
      protected def httpsConfig = h
      protected lazy val userAndPassword = up
      protected lazy val name = label
      protected lazy val prefixedUri = baseUri / "agent"

      override def toString = s"AgentClient($prefixedUri)"

  def resource(
    admission: Admission,
    label: String = "Agent",
    httpsConfig: => HttpsConfig = HttpsConfig.empty)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, AgentClient] =
    Resource.make(
      acquire = Task(apply(admission, label, httpsConfig)))(
      release = client => client.tryLogout *> Task(client.close()))
