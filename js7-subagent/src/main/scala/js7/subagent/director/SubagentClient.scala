package js7.subagent.director

import akka.actor.ActorSystem
import io.circe.Decoder
import js7.base.auth.Admission
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.web.Uri
import js7.common.http.AkkaHttpClient
import js7.common.http.Uris.encodeQuery
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

final class SubagentClient(
  admission: Admission,
  protected val httpsConfig: HttpsConfig = HttpsConfig.empty,
  protected val name: String,
  protected val actorSystem: ActorSystem)
extends /*EventApi with*/ SessionApi.HasUserAndPassword with HttpSessionApi with AkkaHttpClient
{
  import admission.uri

  protected def uriPrefixPath = "/subagent"

  protected def baseUri = admission.uri

  private val apiUri = uri / "subagent" / "api"
  protected val sessionUri = apiUri / "session"
  private val commandUri = apiUri / "command"
  private val eventUri = apiUri / "event"

  protected val httpClient = this

  protected def userAndPassword = admission.userAndPassword

  def executeSubagentCommand[A <: SubagentCommand](numbered: Numbered[A])
  : Task[numbered.value.Response] =
    retryIfSessionLost()(
      httpClient
        .post[Numbered[SubagentCommand], SubagentCommand.Response](
          commandUri,
          numbered.asInstanceOf[Numbered[SubagentCommand]])
        .map(_.asInstanceOf[numbered.value.Response]))

  def eventObservable[E <: Event: ClassTag](
    request: EventRequest[E],
    subagentRunId: SubagentRunId,
    heartbeat: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    retryIfSessionLost()(
      httpClient.getDecodedLinesObservable[Stamped[KeyedEvent[E]]](
        Uri(
          eventUri.string +
            encodeQuery(
              Some("subagentRunId" -> subagentRunId.string) ++
                (heartbeat.map("heartbeat" -> _.toDecimalString) ++
                request.toQueryParameters))),
        responsive = true))
}
