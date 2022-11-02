package js7.service.pgp

import js7.base.crypt.SignatureService

// A Java service provider, published in META-INF/services/js7.base.crypt.SignatureService
final class PgpService extends SignatureService
{
  def verifierCompanion = PgpSignatureVerifier

  def maybeSignerCompanion = Some(PgpSigner)
}
