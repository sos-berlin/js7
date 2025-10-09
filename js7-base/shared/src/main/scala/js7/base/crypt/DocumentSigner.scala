package js7.base.crypt

import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*

trait DocumentSigner:

  protected type MySignature <: Signature

  //def companion: DocumentSigner.Companion { type MySignature = DocumentSigner.this.MySignature }

  def sign(document: ByteArray): MySignature

  final def signString(document: String): MySignature =
    sign(ByteArray(document))

  final def toSignedString(document:String): SignedString =
    SignedString(document, signString(document).toGenericSignature)

  def toLongString: String =
    toString


object DocumentSigner:
  trait Companion:
    protected type MySignature <: Signature   //= MyMessageSigner#MySignature
    protected type MyMessageSigner <: DocumentSigner

    def typeName: String

    def checked(privateKey: ByteArray, password: SecretString): Checked[MyMessageSigner]

    override def toString =
      s"""${getClass.simpleScalaName}("$typeName")"""
