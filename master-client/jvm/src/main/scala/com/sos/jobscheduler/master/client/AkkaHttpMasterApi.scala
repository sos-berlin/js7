package com.sos.jobscheduler.master.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.https.AkkaHttps.loadHttpsConnectionContext
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.typesafe.config.{Config, ConfigFactory}

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApi(
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

  closer onClose { super[CommonAkka].close() }
  closeOnError(closer) {
    actorSystem  // Initialize eagerly to avoid "ClassCastException: interface akka.event.LoggingFilter is not assignable from class akka.event.slf4j.Slf4jLoggingFilter"
  }

  override def close() = closer.close()

  override def toString = s"AkkaHttpMasterApi($baseUri)"
}

object AkkaHttpMasterApi
{
  def apply(
    baseUri: Uri,
    keyStoreRef: Option[KeyStoreRef] = None,
    trustStoreRef: Option[TrustStoreRef] = None)
    (implicit actorSystem: ActorSystem)
  : CommonAkka = {
    val baseUri_ = baseUri
    val actorSystem_ = actorSystem
    val keyStoreRef_ = keyStoreRef
    val trustStoreRef_ = trustStoreRef
    new CommonAkka {
      protected val actorSystem = actorSystem_
      protected val baseUri = baseUri_
      override protected def keyStoreRef = keyStoreRef_
      override protected def trustStoreRef = trustStoreRef_
    }
  }

  trait CommonAkka extends HttpMasterApi with AkkaHttpClient
  {
    /** To provide a client certificate to server. */
    protected def keyStoreRef: Option[KeyStoreRef] = None
    /** To trust the server's certificate. */
    protected def trustStoreRef: Option[TrustStoreRef] = None
    protected final lazy val baseUriString = baseUri.toString

    override protected final lazy val httpsConnectionContextOption =
      (keyStoreRef.nonEmpty || trustStoreRef.nonEmpty) ? loadHttpsConnectionContext(keyStoreRef, trustStoreRef)  // TODO None means HttpsConnectionContext? Or empty context?

    protected final def httpClient = this
  }
}
