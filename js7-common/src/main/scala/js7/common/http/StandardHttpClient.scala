package js7.common.http

import cats.effect.{IO, ResourceIO}
import js7.base.auth.Admission
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionApi
import js7.common.http.PekkoHttpClient
import js7.data.session.HttpSessionApi
import org.apache.pekko.actor.ActorSystem

final class StandardHttpClient private(
  admission: Admission,
  uriPath: String,
  protected val httpsConfig: HttpsConfig,
  protected val name: String)(
  using protected val actorSystem: ActorSystem)
extends
  SessionApi.HasUserAndPassword, HttpSessionApi, PekkoHttpClient:

  import admission.uri

  def isLocal = false

  protected def baseUri = admission.uri

  protected def uriPrefixPath = uriPath

  private val apiUri = uri / uriPath / "api"
  protected val sessionUri = apiUri / "session"
  //private val commandUri = apiUri / "command"
  //private val eventUri = apiUri / "event"

  protected val httpClient = this

  protected def userAndPassword = admission.userAndPassword

  //def executeCommand[Cmd <: CommonCommand](command: Cmd): IO[Checked[command.Response]] =
  //  loginAndRetryIfSessionLost:
  //    liftProblem:
  //      httpClient
  //        .post[Cmd, CommonCommand.Response](commandUri, cmd)
  //        .map(_.asInstanceOf[command.Response])


object StandardHttpClient:

  def resource(
    admission: Admission,
    uriPath: String,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    label: String)
    (using actorSystem: ActorSystem)
  : ResourceIO[StandardHttpClient] =
    SessionApi.logoutOnRelease:
      IO:
        StandardHttpClient(admission, uriPath, httpsConfig, name = label)
