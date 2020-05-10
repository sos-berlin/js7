package com.sos.jobscheduler.master.client

import akka.actor.ActorSystem
import cats.effect.Resource
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.typesafe.config.{Config, ConfigFactory}
import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

object AkkaHttpMasterApi
{
  def apply(
    baseUri: Uri,
    userAndPassword: Option[UserAndPassword],
    actorSystem: ActorSystem,
    config: Config = ConfigFactory.empty,
    /** To provide a client certificate to server. */
    keyStoreRef: Option[KeyStoreRef] = None,
    /** To trust the server's certificate. */
    trustStoreRef: Option[TrustStoreRef] = None,
    retryDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays,
    name: String = "")
  : HttpMasterApi with AutoCloseable =
    new Standard(baseUri, userAndPassword, actorSystem, config, keyStoreRef, trustStoreRef, retryDelays, name)

  class Standard(
    val baseUri: Uri,
    protected val userAndPassword: Option[UserAndPassword],
    actorSystem: ActorSystem,
    protected val config: Config = ConfigFactory.empty,
    /** To provide a client certificate to server. */
    keyStoreRef: Option[KeyStoreRef] = None,
    /** To trust the server's certificate. */
    trustStoreRef: Option[TrustStoreRef] = None,
    retryDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays,
    name: String = "")
  extends HttpMasterApi with AutoCloseable
  {
    final val httpClient = AkkaHttpClient(baseUri, "/master", actorSystem, keyStoreRef, trustStoreRef, name = name)

    override protected def loginDelays() = retryDelays()


    def close() = httpClient.close()
  }

  /** Logs out when the resource is being released. */
  def resource(
    baseUri: Uri,
    userAndPassword: Option[UserAndPassword],
    retryDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays,
    name: String = "")
    (implicit actorSystem: ActorSystem)
  : Resource[Task, Standard] =
    Resource.make(
      acquire = Task(new Standard(baseUri = baseUri, userAndPassword, actorSystem, retryDelays = retryDelays, name = name))
    )(release = api =>
      api.logout()
        .map(_ => ())
        .onErrorHandle(_ => ())
        .guarantee(Task {
          api.close()
        }))
}
