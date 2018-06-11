package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextClient}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.HasCloser

/**
  * @author Joacim Zschimmer
  */
private[master] final class TextMasterClient(
  protected val baseUri: Uri,
  protected val print: String ⇒ Unit,
  protected val userAndPassword: Option[UserAndPassword] = None,
  keystore: Option[KeystoreReference] = None)
extends HasCloser with AkkaHttpClient with ProvideActorSystem with TextClient with SessionApi {

  protected def uriPrefixPath = "/master"

  private val masterUris = MasterUris(s"$baseUri/master")

  protected def httpClient = this

  protected def sessionUri = masterUris.session

  protected def serverName = "JobScheduler Master"

  protected def commandUri = masterUris.command

  protected def apiUri(tail: String) = masterUris.api(tail)

  protected override val httpsConnectionContext = keystore/*Option*/ map Https.toHttpsConnectionContext getOrElse super.httpsConnectionContext

  closer.onClose { super.close() }

  override def close() = closer.close()
}
