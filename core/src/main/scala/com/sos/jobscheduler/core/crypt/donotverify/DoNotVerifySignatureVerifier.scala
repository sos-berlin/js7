package com.sos.jobscheduler.core.crypt.donotverify

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.data.crypt.GenericSignature
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object DoNotVerifySignatureVerifier
extends SignatureVerifier with SignatureVerifier.Companion  // Both Verifier and Companion
{
  protected type MySignature = DoNotVerifySignature.type
  protected type MySignatureVerifier = DoNotVerifySignatureVerifier.type

  def companion = this

  def keyOrigin = "(no signature verification)"

  def recommendedKeyFileName = throw new NotImplementedError("DoNotVerifySignatureVerifier#recommendedKeyFileName")

  def key = throw new NotImplementedError("DoNotVerifySignatureVerifier#key")

  def verify(message: String, signature: DoNotVerifySignature.type) = Valid(Nil)

  def typeName = DoNotVerifySignature.TypeName

  def checked(publicKey: Seq[Byte], keyOrigin: String = keyOrigin) =
    if (publicKey.nonEmpty)
      Invalid(Problem.pure("DoNotVerifySignatureVerifier only accepts an empty public key"))
    else
      Valid(DoNotVerifySignatureVerifier)

  def genericSignatureToSignature(signature: GenericSignature) = DoNotVerifySignature

  override def toString = "DoNotVerifySignatureVerifier"
}
