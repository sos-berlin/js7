package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner
import com.sos.jobscheduler.data.filebased.{FileBased, SignedRepoObject}
import io.circe.Encoder
import org.bouncycastle.openpgp.PGPSecretKey

/**
  * @author Joacim Zschimmer
  */
final class FileBasedSigner private(val pgpSigner: PgpSigner, jsonEncoder: Encoder[FileBased])
{
  def sign(fileBased: FileBased): SignedRepoObject = {
    val message = jsonEncoder(fileBased).compactPrint
    SignedRepoObject(message, pgpSigner.sign(message).toGenericSignature)
  }
}

object FileBasedSigner
{
  def apply(jsonEncoder: Encoder[FileBased], secretKey: PGPSecretKey, password: SecretString): Checked[FileBasedSigner] =
    for (pgpSigner <- PgpSigner(secretKey, password)) yield
      new FileBasedSigner(pgpSigner, jsonEncoder)
}
