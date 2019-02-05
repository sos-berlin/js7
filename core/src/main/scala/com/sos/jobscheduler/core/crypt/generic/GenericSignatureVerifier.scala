package com.sos.jobscheduler.core.crypt.generic

import com.sos.jobscheduler.base.problem.Checked.{CheckedOption, _}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.crypt.configuration.SignatureVerifiers
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignerId}
import com.typesafe.config.Config
import java.nio.file.{Files, Paths}
import javax.inject.{Inject, Singleton}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/** A `SignatureVerifier` that verifies different types of signatures.
  * @author Joacim Zschimmer
  */
@Singleton
final class GenericSignatureVerifier @Inject private[generic](config: Config) extends SignatureVerifier
{
  import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier._

  protected type MySignature = GenericSignature

  def companion = GenericSignatureVerifier

  private val typeToVerifier = config.getObject(configPath).asScala.toMap
    .map { case (typeName, v) =>
      typeName -> (v.unwrapped match {
        case o: String =>
          val file = Paths.get(o)
          if (!Files.exists(file)) throw new IllegalArgumentException(s"Signature key file '$file' for '$typeName' does not exists")
          SignatureVerifiers.typeToSignatureVerifierCompanion
            .getOrElse(typeName, throw new NoSuchElementException(s"Unknown signature provider: $typeName"))
            .apply(Files.readAllBytes(file).toVector)  // throws
        case _ => throw new IllegalArgumentException(s"Invalid value for configurationKey $configPath.$typeName")
      })
    }

  def verify(message: String, signature: GenericSignature): Checked[Seq[SignerId]] =
    typeToVerifier
      .get(signature.typeName)
      .toChecked(Problem.pure(s"Unknown signature type '${signature.typeName}'"))
      .flatMap(verifier => verifier.verify(message, verifier.companion.genericSignatureToSignature(signature)))
}

object GenericSignatureVerifier extends SignatureVerifier.Companion
{
  private val configPath = "jobscheduler.configuration.trusted-signature-keys"

  protected type MySignature = GenericSignature
  protected type MySignatureVerifier = GenericSignatureVerifier

  def typeName = "(generic)"

  def apply(publicKey: Seq[Byte]) = throw new NotImplementedError("GenericSignatureVerifier.apply")

  def genericSignatureToSignature(signature: GenericSignature) = signature
}
