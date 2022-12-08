package js7.controller.client

import akka.actor.ActorSystem
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.io.https.HttpsConfig
import js7.base.session.SessionApi
import js7.base.utils.CatsUtils.Nel
import js7.base.web.Uri
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.http.AkkaHttpClient
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

class AkkaHttpControllerApi(
  val baseUri: Uri,
  protected final val userAndPassword: Option[UserAndPassword],
  actorSystem: ActorSystem,
  protected final val config: Config = ConfigFactory.empty,
  httpsConfig: HttpsConfig = HttpsConfig.empty,
  override protected final val loginDelays: () => Iterator[FiniteDuration] = SessionApi
    .defaultLoginDelays _,
  name: String = "")
extends HttpControllerApi with SessionApi.HasUserAndPassword with AutoCloseable
{
  final val httpClient = new AkkaHttpClient.Standard(
    baseUri, HttpControllerApi.UriPrefixPath, actorSystem, httpsConfig, name = name)

  def close() = {
    logOpenSession()
    httpClient.close()
  }
}

object AkkaHttpControllerApi
{
  private val defaultName = "ControllerApi"

  /** Logs out when the resource is being released. */
  def separateAkkaResource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    config: Config = ConfigFactory.empty,
    name: String = "")
  : Resource[Task, HttpControllerApi] = {
    val myName = if (name.nonEmpty) name else "AkkaHttpControllerApi"
    for {
      actorSystem <- actorSystemResource(name = myName, config)
      api <- resource(uri, userAndPassword, httpsConfig, name = myName)(actorSystem)
    } yield api
  }

  def admissionsToApiResources(
    admissions: Nel[Admission],
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String = defaultName)
    (implicit actorSystem: ActorSystem)
  : Nel[Resource[Task, HttpControllerApi]] =
    for {
      x <- admissions.zipWithIndex
      (a, i) = x
    } yield admissionToApiResource(a, httpsConfig, name = s"$name-$i")

  def admissionToApiResource(
    admission: Admission,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String = defaultName)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, HttpControllerApi] =
    resource(admission.uri, admission.userAndPassword, httpsConfig, name = name)

  /** Logs out when the resource is being released. */
  def resource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _,
    name: String = defaultName)
    (implicit actorSystem: ActorSystem)
  : Resource[Task, HttpControllerApi] =
    for {
      httpClient <- AkkaHttpClient.resource(
        uri, uriPrefixPath = HttpControllerApi.UriPrefixPath,
        httpsConfig, name = name)
      api <- HttpControllerApi.resource(uri, userAndPassword, httpClient, loginDelays)
    } yield api
}
