package com.sos.jobscheduler.core.crypt

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.crypt.{GenericSignature, Signature, SignerId}
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait SignatureVerifier
{
  protected type MySignature <: Signature

  def companion: SignatureVerifier.Companion { type MySignature = SignatureVerifier.this.MySignature }

  def verify(message: String, signature: MySignature): Checked[Seq[SignerId]]
}

object SignatureVerifier
{
  trait Companion
  {
    protected type MySignature <: Signature   //= MySignatureVerifier#MySignature
    protected type MySignatureVerifier <: SignatureVerifier { type MySignature = Companion.this.MySignature }

    def typeName: String

    def apply(publicKey: Seq[Byte]): MySignatureVerifier

    def genericSignatureToSignature(signature: GenericSignature): MySignature
  }
}
