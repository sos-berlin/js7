package js7.base.crypt.generic

import com.typesafe.config.Config
import js7.base.crypt.generic.SignatureProviderRegister.*
import js7.base.crypt.silly.{SillySignatureVerifier, SillySigner}
import js7.base.crypt.x509.{X509SignatureVerifier, X509Signer}
import js7.base.crypt.{DocumentSigner, SignatureService, SignatureVerifier}
import js7.base.log.Logger
import js7.base.system.ServiceProviders.findServices
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.RichIterable

final class SignatureProviderRegister(clock: WallClock, config: Config):

  private val standardVerifierProviders: Seq[SignatureVerifier.Provider] =
    Vector(
      X509SignatureVerifier.Provider(clock, config),
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


object SignatureProviderRegister:
  private val logger = Logger[this.type]
