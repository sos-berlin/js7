package js7.base.crypt.generic

import java.util.ServiceLoader
import js7.base.crypt.silly.{SillySignatureVerifier, SillySigner}
import js7.base.crypt.x509.{X509SignatureVerifier, X509Signer}
import js7.base.crypt.{DocumentSigner, SignatureService, SignatureVerifier}
import js7.base.log.Logger
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.jdk.CollectionConverters.*

object SignatureServices
{
  private val logger = Logger[this.type]

  private val standardVerifiers: Seq[SignatureVerifier.Companion] =
    Vector(
      X509SignatureVerifier,
      SillySignatureVerifier)

  private lazy val standardSigners: Seq[DocumentSigner.Companion] =
    Vector(
      X509Signer,
      SillySigner)

  private lazy val services: Seq[SignatureService] = {
    val serviceLoader = ServiceLoader.load(classOf[SignatureService])

    val iterator = serviceLoader.iterator.asScala
    if (iterator.isEmpty)
      logger.debug("No SignatureService")
    else
      for (service <- iterator/*loads services lazily*/) {
        logger.debug(s"Found service provider ${service.getClass.simpleScalaName}")
      }

    serviceLoader.asScala.toVector
  }

  lazy val nameToSignatureVerifierCompanion: Map[String, SignatureVerifier.Companion] =
    (standardVerifiers ++ services.map(_.verifierCompanion))
      .toKeyedMap(_.typeName)

  lazy val nameToDocumentSignerCompanion: Map[String, DocumentSigner.Companion] =
    (standardSigners ++ services.flatMap(_.maybeSignerCompanion))
      .toKeyedMap(_.typeName)
}
