package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeystoreReference}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.{AkkaHttpClient, TextClient}
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import java.nio.file.Files.exists
import java.nio.file.Path

/**
  * @author Joacim Zschimmer
  */
private[master] final class TextMasterClient(
  protected val baseUri: Uri,
  protected val print: String ⇒ Unit,
  configDirectory: Option[Path] = None)
extends HasCloser with AkkaHttpClient with ProvideActorSystem with TextClient with SessionApi {

  protected def uriPrefixPath = "/master"

  private val masterUris = MasterUris(s"$baseUri/master")

  protected def httpClient = this

  protected def sessionUri = masterUris.session

  protected def serverName = "JobScheduler Master"

  protected def commandUri = masterUris.command

  protected def apiUri(tail: String) = masterUris.api(tail)

  private lazy val keystoreReferenceOption =
    for {
      dir ← configDirectory
      file = dir / "public-https.jks" if exists(file)
    } yield
      KeystoreReference(
        file.toURI.toURL,
        storePassword = Some(SecretString("jobscheduler")),
        keyPassword = Some(SecretString("jobscheduler")))

  protected override val httpsConnectionContext =
    keystoreReferenceOption.fold(super.httpsConnectionContext)(Https.toHttpsConnectionContext)

  closer.onClose { super.close() }

  override def close() = closer.close()
}
