package js7.base.crypt.generic

import js7.base.crypt.silly.{SillySignatureVerifier, SillySigner}
import js7.base.crypt.x509.{X509SignatureVerifier, X509Signer}
import js7.base.crypt.{DocumentSigner, SignatureService, SignatureVerifier}
import js7.base.log.Logger
import js7.base.system.ServiceProviders.findServices
import js7.base.utils.Collections.implicits.RichIterable

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

  private lazy val services: Seq[SignatureService] =
    findServices[SignatureService] { (logLine, _) =>
      logger.debug(logLine)
    }

  lazy val nameToSignatureVerifierCompanion: Map[String, SignatureVerifier.Companion] =
    (standardVerifiers ++ services.map(_.verifierCompanion))
      .toKeyedMap(_.typeName)

  lazy val nameToDocumentSignerCompanion: Map[String, DocumentSigner.Companion] =
    (standardSigners ++ services.flatMap(_.maybeSignerCompanion))
      .toKeyedMap(_.typeName)
}
