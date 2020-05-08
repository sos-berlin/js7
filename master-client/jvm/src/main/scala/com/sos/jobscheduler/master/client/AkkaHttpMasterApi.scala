package com.sos.jobscheduler.master.client

import akka.actor.ActorSystem
import cats.effect.Resource
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.typesafe.config.{Config, ConfigFactory}
import monix.eval.Task

object AkkaHttpMasterApi
{
  def apply(
    baseUri: Uri,
    actorSystem: ActorSystem,
    config: Config = ConfigFactory.empty,
    /** To provide a client certificate to server. */
    keyStoreRef: Option[KeyStoreRef] = None,
    /** To trust the server's certificate. */
    trustStoreRef: Option[TrustStoreRef] = None,
    name: String = "")
  : HttpMasterApi with AutoCloseable =
    new Standard(baseUri, actorSystem, config, keyStoreRef, trustStoreRef, name)

  class Standard(
    val baseUri: Uri,
    actorSystem: ActorSystem,
    protected val config: Config = ConfigFactory.empty,
    /** To provide a client certificate to server. */
    keyStoreRef: Option[KeyStoreRef] = None,
    /** To trust the server's certificate. */
    trustStoreRef: Option[TrustStoreRef] = None,
    name: String = "")
  extends HttpMasterApi with AutoCloseable
  {
    final val httpClient = AkkaHttpClient(baseUri, "/master", actorSystem, keyStoreRef, trustStoreRef, name = name)

    def close() = httpClient.close()
  }

  /** Logs out when the resource is being released. */
  def resource(baseUri: Uri, name: String = "")(implicit actorSystem: ActorSystem): Resource[Task, HttpMasterApi] =
    Resource.make(
      acquire = Task(new Standard(baseUri = baseUri, actorSystem, name = name))
    )(release = api =>
      api.logout()
        .map(_ => ())
        .onErrorHandle(_ => ())
        .guarantee(Task {
          api.close()
        }))
}
