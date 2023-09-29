package js7.base.crypt.generic

import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*
import com.typesafe.config.Config
import java.nio.file.Files.exists
import java.nio.file.{Files, Paths}
import js7.base.Problems.UnknownSignatureTypeProblem
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.JavaCollections.syntax.*
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import scala.collection.immutable
import scala.jdk.CollectionConverters.*

/** A `SignatureVerifier` that verifies different types of signatures.
  * @author Joacim Zschimmer
  */
final class GenericSignatureVerifier private[generic](
  verifiers: immutable.Iterable[SignatureVerifier])
extends SignatureVerifier
{
  private val typeToVerifier = verifiers.toKeyedMap(_.companion.typeName)

  protected type MySignature = GenericSignature

  def companion = GenericSignatureVerifier

  def publicKeys = throw new NotImplementedError("GenericSignatureVerifier#key")

  def publicKeyOrigin = "(GenericSignatureVerifier)"

  override def verify(document: ByteArray, signature: GenericSignature): Checked[Seq[SignerId]] =
    for
      verifier <- typeToVerifier.rightOr(signature.typeName,
        Problem(s"No trusted public key for signature type '${signature.typeName}'"))
      typedSignature <- verifier.companion.genericSignatureToSignature(signature)
      signerIds <- verifier.verify(document, typedSignature)
    yield signerIds

  def isEmpty = typeToVerifier.isEmpty

  override def publicKeysToStrings =
    "GenericSignatureVerifier#publicKeysToStrings" :: Nil
}

object GenericSignatureVerifier extends SignatureVerifier.Companion
{
  private val configPath = "js7.configuration.trusted-signature-keys"
  private val logger = Logger[this.type]

  protected type MySignature = GenericSignature
  protected type MySignatureVerifier = GenericSignatureVerifier

  def typeName = "(generic)"

  def filenameExtension =
    throw new NotImplementedError

  def recommendedKeyDirectoryName =
    throw new NotImplementedError("GenericSignatureVerifier recommendedKeyDirectoryName")

  def checked(config: Config): Checked[GenericSignatureVerifier] =
    config.getObject(configPath).asScala.toMap  // All Config key-values
      .map { case (typeName, v) =>
        typeName ->
          checkedCast[String](v.unwrapped, ConfigStringExpectedProblem(s"$configPath.$typeName"))
            .map(Paths.get(_))
            .flatMap(directory => SignatureServices
              .nameToSignatureVerifierCompanion
              .rightOr(typeName, UnknownSignatureTypeProblem(typeName))
              .flatMap(companion =>
                if !exists(directory) then
                  Left(Problem.pure(
                    s"Signature key directory '$directory' for '$typeName' does not exists"))
                else {
                  val files = autoClosing(Files.list(directory))(_
                    .asScala.filterNot(_.getFileName.toString.startsWith(".")).toVector)
                  if files.isEmpty then {
                    logger.warn(
                      s"No public key files for signature verifier '${companion.typeName}' in directory '$directory'")
                  }
                  companion.checked(files.map(_.byteArray), origin = directory.toString)
                }))
      }
      .toVector
      .traverse {
        case (_, Left(p)) => Left(p)
        case (k, Right(v)) => Right(v)
      }
      .flatMap { verifiers =>
        if verifiers.isEmpty then
          Left(Problem.pure(s"No trusted signature keys - Configure one with $configPath!"))
        else {
          logVerifiers(verifiers)
          Right(
            new GenericSignatureVerifier(verifiers))
        }
      }

  @deprecated("Not implemented", "")
  def checked(publicKeys: Seq[ByteArray], origin: String) =
    throw new NotImplementedError("GenericSignatureVerifier.checked?")

  def genericSignatureToSignature(signature: GenericSignature) =
    Right(signature)

  private def logVerifiers(verifiers: Seq[SignatureVerifier]): Unit =
    if verifiers.isEmpty then
      logger.info("Trusting NO public signature keys")
    else {
      logger.info(
        Seq("Trusting public signature keys:")
          .concat(verifiers
            .sortBy(_.companion.typeName)
            .flatMap(_.publicKeysToStrings))
          .mkString("\n  "))
    }

  private case class ConfigStringExpectedProblem(configKey: String) extends Problem.Lazy(
    s"String expected as value of configuration key $configKey")
}
