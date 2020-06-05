package js7.base.crypt

import js7.base.generic.SecretString
import js7.base.problem.Checked

trait MessageSigner
{
  protected type MySignature <: Signature

  def companion: MessageSigner.Companion { type MySignature = MessageSigner.this.MySignature }

  def sign(message: String): MySignature

  def privateKey: Seq[Byte]

  def publicKey: Seq[Byte]

  def toVerifier: SignatureVerifier { type MySignature = MessageSigner.this.MySignature }

  def verifierCompanion: SignatureVerifier.Companion { type MySignature = MessageSigner.this.MySignature }
}

object MessageSigner
{
  trait Companion
  {
    protected type MySignature <: Signature   //= MyMessageSigner#MySignature
    protected type MyMessageSigner <: MessageSigner

    def typeName: String

    def checked(privateKey: collection.Seq[Byte], password: SecretString): Checked[MyMessageSigner]
  }
}
