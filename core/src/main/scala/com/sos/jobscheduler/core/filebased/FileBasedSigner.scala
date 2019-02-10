package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.core.crypt.MessageSigner
import com.sos.jobscheduler.data.crypt.{Signed, SignedString}
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.Encoder

/**
  * @author Joacim Zschimmer
  */
final class FileBasedSigner(val signer: MessageSigner, jsonEncoder: Encoder[FileBased])
{
  def toSigned(fileBased: FileBased): Signed[FileBased] =
    Signed(fileBased, sign(fileBased))

  def sign(fileBased: FileBased): SignedString = {
    val string = jsonEncoder(fileBased).compactPrint
    SignedString(string, signer.sign(string).toGenericSignature)
  }
}
