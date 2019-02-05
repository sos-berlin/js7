package com.sos.jobscheduler.core.crypt.empty

import cats.data.Validated.Valid
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.data.crypt.GenericSignature
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object EmptySignatureVerifier
extends SignatureVerifier with SignatureVerifier.Companion  // Both Verifier and Companion
{
  protected type MySignature = EmptySignature.type
  protected type MySignatureVerifier = EmptySignatureVerifier.type

  def companion = this

  def verify(message: String, signature: EmptySignature.type) = Valid(Nil)

  def typeName = "Empty"

  def apply(publicKey: Seq[Byte]) = {
    require(publicKey.isEmpty)
    EmptySignatureVerifier
  }

  def genericSignatureToSignature(signature: GenericSignature) = EmptySignature
}
