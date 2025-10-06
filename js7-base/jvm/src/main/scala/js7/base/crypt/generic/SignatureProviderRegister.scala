package js7.base.crypt.generic

import js7.base.crypt.silly.{SillySignatureVerifier, SillySigner}
import js7.base.crypt.x509.{X509SignatureVerifier, X509Signer}
import js7.base.crypt.{DocumentSigner, SignatureService, SignatureVerifier}
import js7.base.log.Logger
import js7.base.system.ServiceProviders.findServices
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.RichIterable

final class SignatureProviderRegister(clock: WallClock):

  private val logger = Logger[this.type]

  private val standardVerifierProviders: Seq[SignatureVerifier.Provider] =
    Vector(
      X509SignatureVerifier.Provider(clock),
      SillySignatureVerifier)

  private lazy val standardSigners: Seq[DocumentSigner.Companion] =
    Vector(
      X509Signer,
      SillySigner)

  private lazy val providers: Seq[SignatureService] =
    findServices[SignatureService]: (logLine, _) =>
      logger.debug(logLine)

  lazy val nameToSignatureVerifierProvider: Map[String, SignatureVerifier.Provider] =
    (standardVerifierProviders ++ providers.map(_.verifierProvider))
      .toKeyedMap(_.typeName)

  lazy val nameToDocumentSignerCompanion: Map[String, DocumentSigner.Companion] =
    (standardSigners ++ providers.flatMap(_.maybeSignerCompanion))
      .toKeyedMap(_.typeName)
