package com.sos.jobscheduler.core.filebased

import cats.effect.{Resource, SyncIO}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.core.signature.PGPSigner
import com.sos.jobscheduler.data.filebased.{FileBased, SignedRepoObject}
import io.circe.Encoder
import java.io.InputStream
import java.util.Base64

/**
  * @author Joacim Zschimmer
  */
final class FileBasedSigner(jsonEncoder: Encoder[FileBased], secretKey: Resource[SyncIO, InputStream], password: SecretString)
{
  val pgpSigner = PGPSigner(secretKey, password)

  def sign(fileBased: FileBased): SignedRepoObject = {
    val message = jsonEncoder(fileBased).compactPrint
    val signature = pgpSigner.sign(stringToInputStreamResource(message))
    SignedRepoObject(message, "PGP", Base64.getMimeEncoder.encodeToString(signature))
  }
}
