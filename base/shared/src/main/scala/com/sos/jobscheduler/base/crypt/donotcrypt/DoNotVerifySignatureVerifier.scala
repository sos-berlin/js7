package com.sos.jobscheduler.base.crypt.donotcrypt

import cats.effect.{Resource, SyncIO}
import com.sos.jobscheduler.base.crypt.{GenericSignature, SignatureVerifier}
import com.sos.jobscheduler.base.problem.Problem
import java.io.InputStream

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

  def recommendedKeyDirectoryName = throw new NotImplementedError("DoNotVerifySignatureVerifier recommendedKeyDirectoryName")

  def keys = throw new NotImplementedError("DoNotVerifySignatureVerifier#key")

  def verify(message: String, signature: DoNotVerifySignature.type) = Right(Nil)

  def typeName = DoNotVerifySignature.TypeName

  def checked(publicKeyRings: Seq[Resource[SyncIO, InputStream]], keyOrigin: String = keyOrigin) =
    if (publicKeyRings.nonEmpty)
      Left(Problem.pure("DoNotVerifySignatureVerifier only accepts an empty public key collection"))
    else
      Right(DoNotVerifySignatureVerifier)

  def genericSignatureToSignature(signature: GenericSignature) = DoNotVerifySignature

  override def toString = "DoNotVerifySignatureVerifier"

  def trustedKeysToString = typeName
}
