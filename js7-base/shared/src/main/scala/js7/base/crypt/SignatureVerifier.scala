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

  def companion: SignatureVerifier.Companion:
    type MySignature = self.MySignature

  def allowExpiredCert: Boolean

  @TestOnly
  def publicKeys: Seq[String]

  def publicKeyOrigin: String

  def publicKeysToStrings: Seq[String]

  def verify(document: ByteArray, signature: MySignature)
  : Checked[Seq[SignerId]]

  final def verify(signed: SignedString): Checked[Seq[SignerId]] =
    companion.genericSignatureToSignature(signed.signature, allowExpiredCert = allowExpiredCert)
      .flatMap(signature =>
        verify(ByteArray(signed.string), signature))

  final def verifyString(document: String, signature: MySignature)
  : Checked[Seq[SignerId]] =
    verify(ByteArray(document), signature)


object SignatureVerifier:
  trait Companion:
    self =>

    protected type MySignature <: Signature   // = MySignatureVerifier#MySignature
    protected type MySignatureVerifier <: SignatureVerifier { type MySignature = self.MySignature }

    def typeName: String

    def filenameExtension: String

    def recommendedKeyDirectoryName: String

    def checked(
      publicKeys: Seq[Labeled[ByteArray]],
      origin: String = "(unknown source)",
      allowExpiredCert: Boolean)
    : Checked[MySignatureVerifier]

    def ignoreInvalid(pems: Seq[Labeled[ByteArray]], origin: String, allowExpiredCert: Boolean)
    : MySignatureVerifier

    def genericSignatureToSignature(signature: GenericSignature, allowExpiredCert: Boolean)
    : Checked[MySignature]

    override def toString =
      s"""${getClass.simpleScalaName}("$typeName")"""
