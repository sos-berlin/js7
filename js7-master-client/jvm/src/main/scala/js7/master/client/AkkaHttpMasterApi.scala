package js7.master.client

import akka.actor.ActorSystem
import cats.effect.Resource
import com.typesafe.config.{Config, ConfigFactory}
import js7.base.auth.UserAndPassword
import js7.base.session.SessionApi
import js7.base.web.Uri
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkautils.Akkas.actorSystemResource
import js7.common.http.AkkaHttpClient
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

class AkkaHttpMasterApi(
  val baseUri: Uri,
  protected final val userAndPassword: Option[UserAndPassword],
  actorSystem: ActorSystem,
  protected final val config: Config = ConfigFactory.empty,
  /** To provide a client certificate to server. */
  keyStoreRef: Option[KeyStoreRef] = None,
  /** To trust the server's certificate. */
  trustStoreRefs: Seq[TrustStoreRef] = Nil,
  override protected final val loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays,
  name: String = "")
extends HttpMasterApi with AutoCloseable
{
  final val httpClient = new AkkaHttpClient.Standard(
    baseUri, HttpMasterApi.UriPrefixPath, actorSystem, keyStoreRef, trustStoreRefs, name = name)

  def close() = httpClient.close()
}

object AkkaHttpMasterApi
{
  /** Logs out when the resource is being released. */
  def separateAkkaResource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    config: Config = ConfigFactory.empty,
    name: String = "")
  : Resource[Task, HttpMasterApi] = {
    val myName = if (name.nonEmpty) name else "AkkaHttpMasterApi"
    for {
      actorSystem <- actorSystemResource(name = myName, config)
      api <- resource(uri, userAndPassword, name = myName)(actorSystem)
    } yield api
  }

  /** Logs out when the resource is being released. */
  def resource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    keyStoreRef: Option[KeyStoreRef] = None,
    trustStoreRefs: Seq[TrustStoreRef] = Nil,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays,
    name: String = "")
    (implicit actorSystem: ActorSystem)
  : Resource[Task, HttpMasterApi] =
    for {
      httpClient <- Resource.fromAutoCloseable(Task(
        new AkkaHttpClient.Standard(uri, HttpMasterApi.UriPrefixPath, actorSystem, keyStoreRef, trustStoreRefs, name = name)))
      api <- HttpMasterApi.resource(uri, userAndPassword, httpClient, loginDelays)
    } yield api
}
