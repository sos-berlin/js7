package js7.subagent.director

import akka.actor.ActorSystem
import cats.effect.Resource
import io.circe.Decoder
import js7.base.auth.Admission
import js7.base.io.https.HttpsConfig
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.stream.Numbered
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.web.Uri
import js7.base.web.Uris.encodeQuery
import js7.common.http.AkkaHttpClient
import js7.data.event.{Event, EventRequest, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
import js7.data.subagent.{SubagentCommand, SubagentRunId}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

final class HttpSubagentApi private(
  admission: Admission,
  protected val httpsConfig: HttpsConfig = HttpsConfig.empty,
  protected val name: String,
  protected val actorSystem: ActorSystem)
extends SubagentApi
with SessionApi.HasUserAndPassword
with HttpSessionApi
with AkkaHttpClient:
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
  : Task[Checked[numbered.value.Response]] =
    liftProblem(retryIfSessionLost()(
      httpClient
        .post[Numbered[SubagentCommand], SubagentCommand.Response](
          commandUri,
          numbered.asInstanceOf[Numbered[SubagentCommand]])
        .map(_.asInstanceOf[numbered.value.Response])))

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

object HttpSubagentApi:
  def resource(
    admission: Admission,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String,
    actorSystem: ActorSystem)
  : Resource[Task, HttpSubagentApi] =
    SessionApi.resource(Task(
      new HttpSubagentApi(admission, httpsConfig, name, actorSystem)))
