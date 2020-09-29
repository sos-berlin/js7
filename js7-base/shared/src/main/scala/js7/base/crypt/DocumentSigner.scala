package js7.base.crypt

import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.problem.Checked

trait DocumentSigner
{
  protected type MySignature <: Signature

  def companion: DocumentSigner.Companion { type MySignature = DocumentSigner.this.MySignature }

  def sign(document: ByteArray): MySignature

  final def signString(document: String) =
    sign(ByteArray(document))
}

object DocumentSigner
{
  trait Companion
  {
    protected type MySignature <: Signature   //= MyMessageSigner#MySignature
    protected type MyMessageSigner <: DocumentSigner

    def typeName: String

    def checked(privateKey: ByteArray, password: SecretString): Checked[MyMessageSigner]
  }
}
