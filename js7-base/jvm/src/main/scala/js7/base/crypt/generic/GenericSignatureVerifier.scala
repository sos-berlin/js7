package js7.base.crypt.generic

import cats.data.NonEmptyVector
import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import java.nio.file.Files.exists
import java.nio.file.{Files, Path, Paths}
import js7.base.Problems.UnknownSignatureTypeProblem
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.WallClock
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.JavaCollections.syntax.*
import js7.base.utils.Labeled
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import scala.collection.immutable
import scala.jdk.CollectionConverters.*
import scala.util.Left

/** A `SignatureVerifier` that verifies different types of signatures.
  * @author Joacim Zschimmer
  */
final class GenericSignatureVerifier private[generic](
  verifiers: immutable.Iterable[SignatureVerifier],
  val provider: GenericSignatureVerifier.Provider)
extends SignatureVerifier:

  private val typeToVerifier = verifiers.toKeyedMap(_.provider.typeName)

  protected type MySignature = GenericSignature

  def publicKeys: Seq[String] =
    throw new NotImplementedError("GenericSignatureVerifier#key")

  def publicKeyOrigin: String =
    "(GenericSignatureVerifier)"

  def verify(document: ByteArray, signature: GenericSignature): Checked[Seq[SignerId]] =
    for
      verifier <- typeToVerifier.rightOr(signature.typeName,
        Problem(s"No trusted public key for signature type '${signature.typeName}'"))
      typedSignature <- verifier.provider.genericSignatureToSignature(signature)
      signerIds <- verifier.verify(document, typedSignature)
    yield
      signerIds

  def isEmpty: Boolean =
    typeToVerifier.isEmpty

  override def publicKeysToStrings: Seq[String] =
    "GenericSignatureVerifier#publicKeysToStrings" :: Nil


object GenericSignatureVerifier:
  private val configPath = "js7.configuration.trusted-signature-keys"
  private val logger = Logger[this.type]

  final class Provider(clock: WallClock, config: Config)
  extends SignatureVerifier.Provider:

    protected type MySignature = GenericSignature
    protected type MySignatureVerifier = GenericSignatureVerifier

    private val signatureProviderRegister = SignatureProviderRegister(clock, config)

    def typeName = "(generic)"

    def filenameExtension: String =
      throw new NotImplementedError

    def recommendedKeyDirectoryName: String =
      throw new NotImplementedError("GenericSignatureVerifier recommendedKeyDirectoryName")

    def readConfiguredVerifiers: Checked[GenericSignatureVerifier] =
      readNonEmptyProviderToDirectory.flatMap:
        _.toVector.traverse: (provider, directory) =>
          readVerifier(provider, directory)
      .flatMap: verifiers =>
        logVerifiers(verifiers)
        Right:
          GenericSignatureVerifier(verifiers, this)

    private def readVerifier(provider: SignatureVerifier.Provider, directory: Path)
    : Checked[provider.MySignatureVerifier] =
      val files = autoClosing(Files.list(directory)):
        _.asScala.filterNot(_.getFileName.toString.startsWith(".")).toVector
      if files.isEmpty then
        logger.warn(s"No public key files for signature verifier '${provider.typeName
          }' in directory '$directory'")
      provider.checked(
        files.map(_.labeledByteArray),
        origin = directory.toString)


    def readNonEmptyProviderToDirectory: Checked[NonEmptyVector[(SignatureVerifier.Provider, Path)]] =
      readProvidersAndDirectories.flatMap: seq =>
        NonEmptyVector.fromVector(seq).toRight:
          Problem.pure(s"No trusted signature keys - Configure one with $configPath!")

    def readProvidersAndDirectories: Checked[Vector[(SignatureVerifier.Provider, Path)]] =
      config.getObject(configPath).asScala.toVector.traverse: (typeName, v) => // All Config key-values
        checkedCast[String](v.unwrapped, ConfigStringExpectedProblem(s"$configPath.$typeName"))
          .map(Paths.get(_))
          .flatMap: directory =>
            signatureProviderRegister.nameToSignatureVerifierProvider
              .rightOr(typeName, UnknownSignatureTypeProblem(typeName))
              .flatMap: provider =>
                if !exists(directory) then
                  Left(Problem.pure:
                    s"Signature key directory '$directory' for '$typeName' does not exist")
                else
                  Right(provider -> directory)

    @deprecated("Not implemented", "")
    def checked(publicKeys: Seq[Labeled[ByteArray]], origin: String) =
      throw new NotImplementedError("GenericSignatureVerifier.checked?")

    @deprecated("Not implemented", "")
    def ignoreInvalid(publicKeys: Seq[Labeled[ByteArray]], origin: String) =
      throw new NotImplementedError("GenericSignatureVerifier.ignoreInvalid?")

    def genericSignatureToSignature(signature: GenericSignature): Checked[GenericSignature] =
      Right(signature)

    private def logVerifiers(verifiers: Seq[SignatureVerifier]): Unit =
      if verifiers.isEmpty then
        logger.info("Trusting NO public signature keys")
      else
        logger.info(
          Seq("Trusting public signature keys:")
            .concat(verifiers
              .sortBy(_.provider.typeName)
              .flatMap(_.publicKeysToStrings))
            .mkString("\n  "))


  private case class ConfigStringExpectedProblem(configKey: String) extends Problem.Lazy(
    s"String expected as value of configuration key $configKey")
