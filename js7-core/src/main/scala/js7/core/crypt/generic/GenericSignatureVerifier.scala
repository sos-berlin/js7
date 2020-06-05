package js7.core.crypt.generic

import cats.Applicative
import cats.effect.{Resource, SyncIO}
import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Collections._
import js7.base.utils.JavaCollections.syntax._
import js7.base.utils.ScalaUtils.checkedCast
import js7.common.scalautil.JavaSyncResources.fileAsResource
import js7.common.scalautil.Logger
import com.typesafe.config.Config
import java.io.InputStream
import java.nio.file.Files.exists
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/** A `SignatureVerifier` that verifies different types of signatures.
  * @author Joacim Zschimmer
  */
final class GenericSignatureVerifier private(typeToVerifier: Map[String, Checked[SignatureVerifier]])
extends SignatureVerifier
{
  protected type MySignature = GenericSignature

  def companion = GenericSignatureVerifier

  def keys = throw new NotImplementedError("GenericSignatureVerifier#key")

  def keyOrigin = "(GenericSignatureVerifier)"

  override def verify(message: String, signature: GenericSignature): Checked[Seq[SignerId]] =
    typeToVerifier(signature.typeName)
      .flatMap(verifier => verifier.verify(message, signature))

  def isEmpty = typeToVerifier.isEmpty

  override def trustedKeysToString = "GenericSignatureVerifier#trustedKeysToString"
}

object GenericSignatureVerifier extends SignatureVerifier.Companion
{
  private val configPath = "js7.configuration.trusted-signature-keys"
  private val logger = Logger(getClass)

  protected type MySignature = GenericSignature
  protected type MySignatureVerifier = GenericSignatureVerifier

  def typeName = "(generic)"

  def recommendedKeyDirectoryName = throw new NotImplementedError("GenericSignatureVerifier recommendedKeyDirectoryName")

  implicitly[Applicative[Checked]]

  def apply(config: Config): Checked[GenericSignatureVerifier] =
    config.getObject(configPath).asScala.toMap  // All Config key-values
      .map { case (typeName, v) =>
        typeName -> (
          checkedCast[String](v.unwrapped, Problem.pure(s"String expected as value of configuration key $configPath.$typeName"))
            .map(Paths.get(_))
            .flatMap(directory =>
              SignatureVerifiers.typeToSignatureVerifierCompanion(typeName).flatMap { companion =>
                if (!exists(directory))
                  Left(Problem.pure(s"Signature key directory '$directory' for '$typeName' does not exists"))
                else {
                  val files = autoClosing(Files.list(directory))(_
                    .asScala.filterNot(_.getFileName.toString.startsWith(".")).toVector)
                  companion.checked(files map fileAsResource, keyOrigin = directory.toString)
                }
              }))
      }
      .toVector.map {
        case (k, Right(v)) => Right(k -> v)
        case (_, Left(p)) => Left(p): Checked[(String, SignatureVerifier)]
      }
      .sequence
      .map(_.toMap)
      .flatMap { typeToVerifier =>
        if (typeToVerifier.isEmpty)
          Left(Problem.pure(s"No trusted signature keys - Configure one with $configPath!"))
        else {
          for (verifier <- typeToVerifier.values.toVector.sortBy(_.companion.typeName)) {
            logger.info(s"Trusting signature keys: ${verifier.trustedKeysToString}")
          }
          Right(
            new GenericSignatureVerifier(
              typeToVerifier.toChecked(key => Problem(s"No trusted public key for signature type '$key'"))))
        }
      }

  @deprecated("Not implemented", "")
  def checked(publicKeyRings: Seq[Resource[SyncIO, InputStream]], keyOrigin: String) =
    throw new NotImplementedError("GenericSignatureVerifier.checked?")

  def genericSignatureToSignature(signature: GenericSignature) =
    signature
}
