package js7.subagent.client

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
import js7.subagent.data.SubagentCommand
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

  protected val sessionUri = uri / "subagent" / "api" / "session"

  protected val httpClient = this

  protected def userAndPassword = admission.userAndPassword

  def executeSubagentCommand(numbered: Numbered[SubagentCommand]): Task[numbered.value.Response] =
    retryIfSessionLost()(
      httpClient
        .post[Numbered[SubagentCommand], SubagentCommand.Response](
          uri / "subagent" / "api" / "command", numbered)
        .map(_.asInstanceOf[numbered.value.Response]))

  def eventObservable[E <: Event: ClassTag](
    request: EventRequest[E],
    heartbeat: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    httpClient.getDecodedLinesObservable[Stamped[KeyedEvent[E]]](
      Uri(
        (uri / "subagent" / "api" / "event").string +
          encodeQuery(
           (heartbeat.map("heartbeat" -> _.toDecimalString)) ++
              request.toQueryParameters)),
      responsive = true)
}
