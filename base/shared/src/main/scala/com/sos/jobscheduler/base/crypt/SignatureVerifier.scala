package com.sos.jobscheduler.base.crypt

import cats.effect.{Resource, SyncIO}
import com.sos.jobscheduler.base.problem.Checked
import java.io.InputStream

/**
  * @author Joacim Zschimmer
  */
trait SignatureVerifier
{
  protected type MySignature <: Signature

  def companion: SignatureVerifier.Companion { type MySignature = SignatureVerifier.this.MySignature }

  def keys: Seq[Seq[Byte]]

  def keyOrigin: String

  def trustedKeysToString: String

  protected def verify(message: String, signature: MySignature): Checked[Seq[SignerId]]

  final def verify(signed: SignedString): Checked[Seq[SignerId]] =
    verify(signed.string, signed.signature)

  def verify(message: String, signature: GenericSignature): Checked[Seq[SignerId]] =
    verify(message, companion.genericSignatureToSignature(signature))
}

object SignatureVerifier
{
  trait Companion
  {
    protected type MySignature <: Signature   //= MySignatureVerifier#MySignature
    protected type MySignatureVerifier <: SignatureVerifier { type MySignature = Companion.this.MySignature }

    def typeName: String

    def recommendedKeyDirectoryName: String

    def checked(publicKeys: Seq[Resource[SyncIO, InputStream]], keyOrigin: String = "(unknown source)"): Checked[MySignatureVerifier]

    def genericSignatureToSignature(signature: GenericSignature): MySignature
  }
}
