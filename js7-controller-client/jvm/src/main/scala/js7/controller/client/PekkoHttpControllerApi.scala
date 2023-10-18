package js7.controller.client

import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionApi
import js7.base.utils.CatsUtils.Nel
import js7.base.web.Uri
import js7.common.http.PekkoHttpClient
import js7.common.pekkoutils.Pekkos.actorSystemResource
import monix.eval.Task
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.FiniteDuration

class PekkoHttpControllerApi(
  val baseUri: Uri,
  protected final val userAndPassword: Option[UserAndPassword],
  actorSystem: ActorSystem,
  protected final val config: Config = ConfigFactory.empty,
  httpsConfig: HttpsConfig = HttpsConfig.empty,
  override protected final val loginDelays: () => Iterator[FiniteDuration] = SessionApi
    .defaultLoginDelays _,
  name: String = "")
extends HttpControllerApi, SessionApi.HasUserAndPassword, AutoCloseable:

  final val httpClient: PekkoHttpClient =
    new PekkoHttpClient.Standard(
      baseUri, HttpControllerApi.UriPrefixPath, actorSystem, httpsConfig, name = name)

  def close() =
    logOpenSession()
    httpClient.close()


object PekkoHttpControllerApi:
  private val defaultName = "ControllerApi"

  /** Logs out when the resource is being released. */
  def separatePekkoResource(
    admission: Admission,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    config: Config = ConfigFactory.empty,
    name: String = "")
  : Resource[Task, HttpControllerApi] =
    val myName = if name.nonEmpty then name else "PekkoHttpControllerApi"
    for
      actorSystem <- actorSystemResource(name = myName, config)
      api <- resource(admission, httpsConfig, name = myName)(actorSystem)
    yield api

  def admissionsToApiResource(
    admissions: Nel[Admission],
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String = defaultName)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, Nel[HttpControllerApi]] =
    admissions.zipWithIndex
      .traverse { case (a, i) => resource(a, httpsConfig, name = s"$name-$i") }

  /** Logs out when the resource is being released. */
  def resource(
    admission: Admission,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _,
    name: String = defaultName)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, HttpControllerApi] =
    for
      httpClient <- PekkoHttpClient.resource(
        admission.uri, uriPrefixPath = HttpControllerApi.UriPrefixPath,
        httpsConfig, name = name)
      api <- HttpControllerApi.resource(admission, httpClient, loginDelays)
    yield api
