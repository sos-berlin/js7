package com.sos.jobscheduler.core.crypt

import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.crypt.Signature
import scala.collection.immutable.Seq

trait MessageSigner
{
  protected type MySignature <: Signature

  def companion: MessageSigner.Companion { type MySignature = MessageSigner.this.MySignature }

  def sign(message: String): MySignature

  def privateKey: Seq[Byte]

  def publicKey: Seq[Byte]
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
