package js7.agent.client

import cats.effect.{IO, Resource}
import cats.syntax.flatMap.*
import fs2.Stream
import js7.agent.client.AgentClient.*
import js7.agent.data.AgentState.keyedEventJsonCodec
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.*
import js7.agent.data.views.AgentOverview
import js7.agent.data.web.AgentUris
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.monixlike.MonixLikeExtensions.tapError
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.web.Uri
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.http.PekkoHttpClient
import js7.data.Problems.ClusterNodeIsNotReadyProblem
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
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
extends HttpSessionApi, PekkoHttpClient, SessionApi.HasUserAndPassword, HttpClusterNodeApi:

  def httpClient = this

  def baseUri: Uri

  //protected lazy val sessionUri = agentUris.session
  protected lazy val agentUris = AgentUris(baseUri)
  protected lazy val uriPrefixPath = "/agent"

  final def repeatUntilAvailable[A](timeout: FiniteDuration)(body: IO[Checked[A]])
  : IO[Checked[A]] =
    IO.defer:
      val sym = new BlockingSymbol
      val delays = Iterator(100.ms, 300.ms, 600.ms) ++ Iterator.continually(1.s)
      val until = now + timeout
      ().tailRecM { _ =>
        body
          .flatMap:
            case Left(problem) if (problem is ClusterNodeIsNotReadyProblem) && now < until =>
              sym.increment()
              logger.log(sym.logLevel, s"$sym $toString: $problem")
              IO.sleep(delays.next()).as(Left(()))

            case checked  =>
              logger.log(
                sym.relievedLogLevel,
                checked match {
                  case Left(problem) => s"❓$toString: $problem"
                  case Right(_) => s"🟢 $toString was available again"
                })
              IO.right(checked)
          .tapError(throwable => IO(
            logger.log(sym.relievedLogLevel, s"💥$toString => $throwable")))
      }

  final def commandExecute(command: AgentCommand): IO[Checked[command.Response]] =
    liftProblem(
      post[AgentCommand, AgentCommand.Response](uri = agentUris.command, command)
        .map(_.asInstanceOf[command.Response]))

  final def overview: IO[AgentOverview] = get[AgentOverview](agentUris.overview)

  final def agentEventStream(
    request: EventRequest[Event],
    heartbeat: Option[FiniteDuration] = None)
  : IO[Checked[Stream[IO, Stamped[KeyedEvent[Event]]]]] =
    liftProblem(
      getDecodedLinesStream[Stamped[KeyedEvent[Event]]](
        agentUris.controllersEvents(
          request,
          heartbeat = heartbeat),
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
  : Resource[IO, AgentClient] =
    Resource.make(
      acquire = IO(apply(admission, label, httpsConfig)))(
      release = client => client.tryLogout *> IO(client.close()))
