package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.https.AkkaHttps.loadHttpsConnectionContext
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.typesafe.config.{Config, ConfigFactory}

/**
  * @author Joacim Zschimmer
  */
class AkkaHttpMasterApi(
  protected val baseUri: Uri,
  /** To provide a client certificate to server. */
  override protected val keyStoreRef: Option[KeyStoreRef] = None,
  /** To trust the server's certificate. */
  override protected val trustStoreRef: Option[TrustStoreRef] = None,
  protected val config: Config = ConfigFactory.empty)
extends AkkaHttpMasterApi.CommonAkka
with ProvideActorSystem
{
  private type CommonAkka = AkkaHttpMasterApi.CommonAkka

  actorSystem  // Initialize eagerly to avoid "ClassCastException: interface akka.event.LoggingFilter is not assignable from class akka.event.slf4j.Slf4jLoggingFilter"
  closer.onClose { super[CommonAkka].close() }

  override def close() = super[ProvideActorSystem].closer.close()
}

object AkkaHttpMasterApi
{
  trait CommonAkka extends HttpMasterApi with AkkaHttpClient
  {
    /** To provide a client certificate to server. */
    protected def keyStoreRef: Option[KeyStoreRef] = None
    /** To trust the server's certificate. */
    protected def trustStoreRef: Option[TrustStoreRef] = None
    protected final val baseUriString = baseUri.toString

    override protected final lazy val httpsConnectionContextOption =
      (keyStoreRef.nonEmpty || trustStoreRef.nonEmpty) ? loadHttpsConnectionContext(keyStoreRef, trustStoreRef)  // TODO None means HttpsConnectionContext? Or empty context?

    protected final def httpClient = this
  }
}
