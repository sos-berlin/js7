package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.core.crypt.MessageSigner
import com.sos.jobscheduler.data.crypt.{Signed, SignedString}
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.Encoder

/**
  * @author Joacim Zschimmer
  */
final class FileBasedSigner[A <: FileBased](val signer: MessageSigner, jsonEncoder: Encoder[A])
{
  def toSigned(fileBased: A): Signed[A] =
    Signed(fileBased, sign(fileBased))

  def sign(fileBased: A): SignedString = {
    val string = jsonEncoder(fileBased).compactPrint
    SignedString(string, signer.sign(string).toGenericSignature)
  }
}
