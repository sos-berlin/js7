package js7.service.pgp

import js7.base.crypt.{DocumentSigner, SignatureService}

// A Java service provider, published in META-INF/services/js7.base.crypt.SignatureService
final class PgpService extends SignatureService:
  def verifierProvider: PgpSignatureVerifier.type = PgpSignatureVerifier

  def maybeSignerCompanion: Some[DocumentSigner.Companion] =
    Some(PgpSigner)

  override def toString: String =
    verifierProvider.typeName
