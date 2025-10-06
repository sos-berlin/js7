package js7.base.crypt

import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.base.utils.Labeled
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
trait SignatureVerifier:
  self =>

  protected type MySignature <: Signature

  def provider: SignatureVerifier.Provider:
    type MySignature = self.MySignature

  @TestOnly
  def publicKeys: Seq[String]

  def publicKeyOrigin: String

  def publicKeysToStrings: Seq[String]

  def verify(document: ByteArray, signature: MySignature): Checked[Seq[SignerId]]

  final def verify(signed: SignedString): Checked[Seq[SignerId]] =
    provider.genericSignatureToSignature(signed.signature)
      .flatMap(signature => verify(ByteArray(signed.string), signature))

  final def verifyString(document: String, signature: MySignature): Checked[Seq[SignerId]] =
    verify(ByteArray(document), signature)


object SignatureVerifier:
  trait Provider:
    self =>

    protected type MySignature <: Signature   // = MySignatureVerifier#MySignature
    protected type MySignatureVerifier <: SignatureVerifier { type MySignature = self.MySignature }

    def typeName: String

    def filenameExtension: String

    def recommendedKeyDirectoryName: String

    def checked(publicKeys: Seq[Labeled[ByteArray]], origin: String = "(unknown source)")
    : Checked[MySignatureVerifier]

    def ignoreInvalid(pems: Seq[Labeled[ByteArray]], origin: String): MySignatureVerifier

    def genericSignatureToSignature(signature: GenericSignature): Checked[MySignature]

    override def toString =
      s"""${getClass.simpleScalaName}("$typeName")"""
