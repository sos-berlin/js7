package js7.subagent.director

import cats.effect.{IO, ResourceIO}
import fs2.Stream
import io.circe.Decoder
import js7.base.auth.Admission
import js7.base.io.https.HttpsConfig
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.web.Uri
import js7.base.web.Uris.encodeQuery
import js7.common.http.PekkoHttpClient
import js7.data.event.{Event, EventRequest, JournalEvent, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

final class HttpSubagentApi private(
  admission: Admission,
  protected val httpsConfig: HttpsConfig = HttpsConfig.empty,
  protected val name: String,
  protected val actorSystem: ActorSystem)
extends SubagentApi, SessionApi.HasUserAndPassword, HttpSessionApi, PekkoHttpClient:

  import admission.uri

  def isLocal = false

  protected def uriPrefixPath = "/subagent"

  protected def baseUri = admission.uri

  private val apiUri = uri / "subagent" / "api"
  protected val sessionUri = apiUri / "session"
  private val commandUri = apiUri / "command"
  private val eventUri = apiUri / "event"

  protected val httpClient = this

  protected def userAndPassword = admission.userAndPassword

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : IO[Checked[numbered.value.Response]] =
    loginAndRetryIfSessionLost:
      liftProblem:
        httpClient
          .post[Numbered[SubagentCommand], SubagentCommand.Response](
            commandUri,
            numbered.asInstanceOf[Numbered[SubagentCommand]])
          .map(_.asInstanceOf[numbered.value.Response])

  def eventStream[E <: Event: ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId)
    (using Decoder[KeyedEvent[E]])
  : IO[Stream[IO, Stamped[KeyedEvent[E]]]] =
    eventStream(request, subagentRunId, heartbeat = None)

  def eventStream[E <: Event](
    request: EventRequest[E],
    subagentRunId: SubagentRunId,
    heartbeat: Option[FiniteDuration] = None,
    idleTimeout: Option[FiniteDuration] = None,
    serverMetering: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : IO[Stream[IO, Stamped[KeyedEvent[E]]]] =
    loginAndRetryIfSessionLost:
      httpClient.getDecodedLinesStream[Stamped[KeyedEvent[E]]](
        Uri:
          eventUri.string +
            encodeQuery:
              Some("subagentRunId" -> subagentRunId.string) ++
                heartbeat.map("heartbeat" -> _.toDecimalString) ++
                serverMetering.map("serverMetering" -> _.toDecimalString) ++
                request.toQueryParameters,
        returnHeartbeatAs = for _ <- heartbeat yield JournalEvent.StampedHeartbeatByteArray,
        idleTimeout = idleTimeout,
        responsive = true,
        dontLog = true)


object HttpSubagentApi:
  def resource(
    admission: Admission,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String,
    actorSystem: ActorSystem)
  : ResourceIO[HttpSubagentApi] =
    SessionApi.resource:
      IO:
        HttpSubagentApi(admission, httpsConfig, name, actorSystem)
